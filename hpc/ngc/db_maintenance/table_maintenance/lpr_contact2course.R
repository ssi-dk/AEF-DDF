#!/usr/bin/env Rscript
#'
#' EpiLPR - contact2course
#'
#' Hver kontakt pr. patient er tilknyttet netop ét forløb.
#' Dette script indsamler data om patientkontakter og bygger en tabel
#' over forløbs-ID'er pr. person

#PBS -d /ngc/people/margru/mail_out

cat(sprintf("[%s] Started\n",
            format(Sys.time(), "%H:%M:%S")))

suppressPackageStartupMessages({
  library(tidyverse)
  library(mg)
})

course_buffer_hours <- 72
fravaer_admin_buffer_hours <- 1

con <- get_connection()
ts <- Sys.getenv("TIMESTAMP", unset = NA)

stopifnot("TIMESTAMP is not defined"=!is.na(ts))

tic <- Sys.time()

target_table <- tryCatch(
  tbl(con, id("mg.lpr_courses_of_contact")),
  error = \(e){ "mg.lpr_courses_of_contact" }
)
orlov <- tbl(con, id("prod.lpr3_sb_opholdsadresse"))

if (inherits(target_table, "tbl_sql")){
  last_update <- target_table %>%
    summarise(ts = dplyr::coalesce(
      max(pmax(from_ts, until_ts, na.rm = T), na.rm = T),
      as.POSIXct("1900-01-01")
    )) %>%
    pull()
} else {
  # Fallback in order to enable creating the table with update_snapshot
  last_update <- as.POSIXct("1990-01-01")
}

# Compute distinct values in background
{
  t_list <- list(
    kontakt_bg = "prod.lpr3_sb_kontakt",
    orlov_bg = "prod.lpr3_sb_opholdsadresse"
  )
  for (x in 1:length(t_list)) {
    t_name <- t_list[[x]]
    job_name <- names(t_list)[x]

    bg_job <- callr::r_bg(\(table_name, last_update, ts) {
      require(mg)
      require(dplyr)
      con <- mg::get_connection()

      table_delta <- dplyr::tbl(con, id(table_name)) %>%
        filter(if_any(c(from_ts, until_ts), ~ . >= !!last_update)) %>%
        {
          .data <- .
          if (as.Date(ts) == as.Date(Sys.time())) {
            .data %>%
              filter(if_any(c(from_ts, until_ts), ~ . <= !!ts))
          } else {
            .data %>%
              filter(if_any(c(from_ts, until_ts), ~ . < !!ts))
          }
        } %>%
        distinct(kontakt_id) %>%
        collect()

      return(table_delta)
    },
    args = list(table_name = t_name,
                last_update = last_update,
                ts = ts))

    cat(sprintf("[%s] Job '%s' started\n",
                format(Sys.time(), "%H:%M:%S"),
                job_name))
    assign(job_name, bg_job, envir = .GlobalEnv)
  }

  kontakt_bg$wait()
  orlov_bg$wait()

  cat(sprintf("[%s] Kontakt_keys received (n=%d); copying...\n",
              format(Sys.time(), "%H:%M:%S"),
              nrow(union(kontakt_bg$get_result(),
                         orlov_bg$get_result()
                         ))))

  kontakt_keys <- copy_to(con,
                          union(kontakt_bg$get_result(), orlov_bg$get_result()),
                          name = "kontakt_keys",
                          overwrite = T)
}

lpr_contact <- get_table(con, "prod.lpr3_sb_kontakt", slice_ts = ts) %>%
  filter_keys(kontakt_keys) %>%
  compute()
orlov <- get_table(con, "prod.lpr3_sb_opholdsadresse", slice_ts = ts) %>%
  filter_keys(kontakt_keys) %>%
  compute()

# Trim orlov-linjer til slut af sidste opholdsadresse uden efterfølgende
# fravær, men kun hvis sidste opholdsadresse er < fravaer_admin_buffer.
# Case: Patient sendes på orlov, afsluttes m. telefonopkald

cat(sprintf("[%s] Computing durations and adjusted end times\n",
            format(Sys.time(), "%H:%M:%S")))

orlov_filtered <- orlov %>%
  mutate(sluttidspunkt = dplyr::coalesce(sluttidspunkt, starttidspunkt)) %>%
  group_by(kontakt_id) %>%
  window_order(desc(sluttidspunkt)) %>%
  mutate(
    varighed = dbplyr::sql_expr(extract(
      "epoch" %from% sluttidspunkt - starttidspunkt
    ), con = con),
    sengeplads = case_when(
      !is.na(fravaer) ~ F,
      sluttidspunkt < max(sluttidspunkt, na.rm = T) ~ T,
      varighed < 3600 * fravaer_admin_buffer_hours & !is.na(lag(fravaer, 1)) ~ F,
      T ~ T
    )
  )

cat(sprintf("[%s] Summarizing end times and sengeplads_andel\n",
            format(Sys.time(), "%H:%M:%S")))

kontakt_prelim <- orlov_filtered %>%
  group_by(kontakt_id) %>%
  summarise(sluttidspunkt_just = dplyr::coalesce(max(sluttidspunkt[sengeplads], na.rm = T), max(sluttidspunkt, na.rm = T)),
            sengeplads_andel = ifelse(
              !is.na(sum(varighed, na.rm = T)) & sum(varighed, na.rm = T) > 0,
              dplyr::coalesce(sum(varighed[sengeplads], na.rm = T) / sum(varighed, na.rm = T), 0),
              0
            )
  ) %>%
  dplyr::right_join(lpr_contact,
                    by = "kontakt_id",
                    na_matches = "never",
                    copy = T) %>%
  relocate(sluttidspunkt_just, .after = sluttidspunkt) %>%
  compute()

cat(sprintf("[%s] konktakt_prelim ready, computing person_keys\n",
            format(Sys.time(), "%H:%M:%S")))

person_keys <- compute(distinct(kontakt_prelim, personnummer))

cat(sprintf("[%s] Summarizing course_id per affected person\n",
            format(Sys.time(), "%H:%M:%S")))

# Compute course ID's and apply to contacts
course_ids <- get_table(con, "prod.lpr3_sb_kontakt", slice_ts = ts) %>%
  filter_keys(person_keys) %>%
  group_by(personnummer) %>%
  window_order(sluttidspunkt) %>%
  transmute(kontakt_id,
            starttidspunkt,
            sluttidspunkt = coalesce(sluttidspunkt, starttidspunkt),
            lag_time = dplyr::EXTRACT("EPOCH" %from% starttidspunkt - lag(sluttidspunkt, n = 1L, default = starttidspunkt)),
            course_id = as.integer(cumsum(ifelse(lag_time > course_buffer_hours * 3600, 1, 0)))) %>%
  select(-lag_time) %>%
  ungroup() %>%
  { if(interactive()) compute(.) else . }

kontakt_prelim <- left_join(kontakt_prelim, select(course_ids, kontakt_id, course_id), by = "kontakt_id")

courses_of_contact <- course_ids %>%
  group_by(personnummer, course_id) %>%
  window_order() %>%
  summarise(forloeb_start = min(starttidspunkt, na.rm = T),
            forloeb_slut = max(sluttidspunkt, na.rm = T),
            .groups = "keep") %>%
  ungroup() %>%
  { if (interactive()) compute(.) else .}

cat(sprintf("[%s] Updating extended contacts table\n",
            format(Sys.time(), "%H:%M:%S")))

update_snapshot(
  kontakt_prelim,
  con,
  "mg.epilpr_contact_expanded",
  filters = kontakt_keys,
  timestamp = ts,
  tic = tic
)

cat(sprintf("[%s] Updating mg.lpr_courses_of_contacts\n",
            format(Sys.time(), "%H:%M:%S")))

update_snapshot(
  .data = courses_of_contact,
  conn = con,
  db_table = "mg.lpr_courses_of_contact",
  filters = person_keys,
  timestamp = ts,
  tic = tic
)
