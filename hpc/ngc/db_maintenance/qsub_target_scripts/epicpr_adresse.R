#!/usr/bin/env Rscript

#PBS -d /ngc/people/margru/mail_out
#PBS -j oe
#PBS -l mem=30g
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:15:00

suppressPackageStartupMessages({
  library(tidyverse)
  library(mg)
})

logmsg <- function(...) {
  cat(format(Sys.time(), "[%H:%M:%S]: "), ..., "\n", sep = "")
}

logmsg("Started!")
tic <- Sys.time()

con <- get_connection()
target_table <- id("mg.epicpr_adresse")

# Check if there is any new data since last run
ts <- Sys.getenv("TIMESTAMP", unset = NA)

if (is.na(ts)){
  last_run <- tbl(con, target_table) %>%
    summarise(ts = coalesce(
      max(pmax(from_ts, until_ts, na.rm = T), na.rm = T),
      "1900-01-01"
    )) %>%
    pull()

  timestamps <- select(tbl(con, id("prod.cpr3_t_adresse")), from_ts) %>%
    union(select(tbl(con, id("prod.cpr3_t_adresse_hist")), from_ts)) %>%
    union(select(tbl(con, id("prod.cpr3_t_person")), from_ts)) %>%
    union(select(tbl(con, id("prod.cpr3_t_sognkod")), from_ts)) %>%
    filter(from_ts > last_run) %>%
    pull() %>%
    sort()

  if (length(timestamps) == 0) {
    if (!interactive()) q(save = "no", status = 0, runLast = FALSE)
  }

  ts <- as.character(timestamps[1])
}

# Define functions for mapping addresses
quantify_address <- function(.data, col, ...){
  .dots <- enquos(col, ...)
  cn <- ncol(.data)  # To avoid mutating existing columns

  stopifnot("Invalid column format (3 digits + 0-1 letters or NULL)" =
              {
                .data %>%
                  filter(across(.cols = {{ col }},
                                .fns = ~ !(is.na(.x) |
                                             grepl("^[[:digit:]]{3}([[:alpha:]])?$", .x))
                  )) %>%
                  count() %>%
                  pull() %>%
                  `==`(0)
              })

  .data %>%
    mutate(across(c(!!!.dots),
                  ~ ifelse(is.na(.), "000", .),
                  .names = "{.col}_num")) %>%
    mutate(across(-c(1:!!cn), ~ case_when(
      is.na(.) ~ 0,
      TRUE ~ as.numeric(paste0(substr(., 1, 3),
                               ".",
                               ascii(substr(., 4, 4))))))
    )
}

map_addresses <- function(addrs, pars, include_num = F) {
  if (!all(sapply(list(addrs, pars), is, "tbl_lazy"))){
    log_error("addrs and pars must be two lazy query objects!")
  }

  adr_num <- quantify_address(addrs, v_husnum)
  par_num <- quantify_address(pars, v_husnrfra, v_husnrtil)

  adr_map <- adr_num %>%
    mutate(v_ligeulige = ifelse(floor(v_husnum_num) %% 2 == 1, "U", "L")) %>%
    inner_join(par_num,
               by = c("c_kom", "c_vej", "v_ligeulige")) %>%
    filter(between(v_husnum_num, v_husnrfra_num, v_husnrtil_num)) %>%
    select(-starts_with(c("v_husnrfra", "v_husnrtil", "v_ligeulige")))

  if(!include_num){
    adr_map <- select(adr_map, -c(ends_with("_num")))
  }

  adr_map
}

cpr <- get_table(con, "prod.cpr3_t_person", slice_ts = ts) %>%
  filter(c_status %in% c("01", "03", "05", "07", "90")) %>%
  distinct() %>%
  compute()

adr <- get_table(con, "prod.cpr3_t_adresse", slice_ts = ts) %>%
  filter(!c_kom %like% "0%", !c_kom %like% "9%") %>%
  distinct() %>%
  compute()

adr_hist <- get_table(con, "prod.cpr3_t_adresse_hist", slice_ts = ts) %>%
  filter(!c_kom %like% "0%", !c_kom %like% "9%", is.na(c_annkor)) %>%
  select(-c_annkor)

par <- get_table(con, "prod.cpr3_t_sognkod", slice_ts = ts) %>%
  filter(!c_kom %like% "0%", !c_kom %like% "9%")

set_rows <- map(list(cpr, adr, adr_hist, par), function(x) pull(count(x)))
if (any(set_rows == 0)){
  log_warn(paste(c("cpr", "adr", "adr_hist", "par")[set_rows == 0],
                 collapse = ", "),
           " have no data, stopping")

  if (!interactive()) q()
}

# Subset address to include only relevant v_pnr's
logmsg("Mapping current addresses")
adr2 <- adr %>%
  inner_join(select(cpr, v_pnr), by = "v_pnr")

adr_map <- map_addresses(adr2, par) %>% compute()

# Get v_pnrs not mapping
logmsg("Getting nomaps")
nomap <- compute(anti_join(adr2, adr_map, by = "v_pnr"))

# Remap v_husnum's out of range
logmsg("Mapping ", pull(count(nomap)), " nomaps")
remap <- nomap %>%
  mutate(v_ligeulige = case_when(
    is.na(v_husnum) ~ "L",
    as.numeric(substr(v_husnum, 3, 3)) %% 2 == 0 ~ "L",
    T ~ "U"
  )) %>%
  left_join(par, by = c("c_kom", "c_vej", "v_ligeulige")) %>%
  quantify_address(v_husnum, v_husnrfra, v_husnrtil) %>%
  mutate(
    v_husnum_num = case_when(
      v_husnum_num < v_husnrfra_num ~ v_husnrfra_num,
      v_husnum_num > v_husnrtil_num ~ v_husnrtil_num)
  ) %>%
  filter(v_husnrfra_num <= v_husnum_num,
         v_husnum_num <= v_husnrtil_num) %>%
  compute()

# Get v_pnrs from cpr with no mappable address
logmsg("Getting CPR's with no address")
noaddr <- cpr %>%
  select(v_pnr) %>%
  anti_join(adr_map, by = "v_pnr") %>%
  anti_join(remap, by = "v_pnr")

logmsg("Computing full (current) mapping")
full_map <- union_all(adr_map, remap, noaddr) %>%
  compute()

stopifnot("Some addresses have not been mapped!"={
  anti_join(adr2, full_map, by = "v_pnr") %>%
    count() %>%
    pull() %>%
    `==`(0)
})

logmsg("Mapping hist addresses")
adr_hist_map <- adr_hist %>%
  map_addresses(par) %>%
  compute()

logmsg("Getting hist nomaps")
anti_join(adr_hist, adr_hist_map, by = c("v_pnr", "d_tilflyt_dato", "d_fraflyt_dato")) %>%
  count() %>%
  pull() %>%
{
  if (. > 0){
    logmsg("Failed to map ", ., " rows in cpr3_t_adresse_hist")
  }
}

# Populate the table
union_query <- union(adr_map, adr_hist_map) %>%
  filter(!c_mynkod == "9999") %>%
  select(colnames(adr_hist_map))

logmsg("Updating table")
update_snapshot(union_query, con, "mg.epicpr_adresse", timestamp = ts, tic = tic)

logmsg("All done!")
DBI::dbDisconnect(con)
