#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=01:15:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : mg.epicpr_c_status, prod.municipalities, prod.epicpr_adresse

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(purrr)
})

# Get the input_args from the environment
target_table <- Sys.getenv("TARGET_TABLE", unset = "mg.epicpr")
slice_ts     <- Sys.getenv("TIMESTAMP",    unset = "missing")
full_mode    <- any(c("--full") %in% commandArgs(trailingOnly = TRUE))

if (slice_ts == "missing") stop()

conn <- get_connection()

# Age cuts
age_cuts_2  <- 60                 # Age groups 0-59, 60+
age_cuts_9  <- (1:8)*10           # Age groups 0-9, 10-19, ... 80+
age_cuts_10 <- c(5, 12, (2:8)*10) # Age groups 0-4, 5-11, 12-19, 20-29, ... 80+
age_cuts = sort(unique(c(age_cuts_2, age_cuts_9, age_cuts_10)))

# Active cpr c_statuses
active_cpr_status <- \(last_update) tbl(conn, id("prod.cpr3_t_person_status")) %>%
  filter(from_ts <= last_update) %>%
  group_by(c_status) %>%
  slice_max(from_ts) %>%
  filter(c_type == "AKTIV") %>%
  select(c_status)

# Check if there are any changes to the input data, and determine which CPR numbers are affected
{
  dependencies <- c("mg.epicpr_c_status", "mg.epicpr_adresse")

  changed_keys <- NULL
  if (table_exists(conn, target_table)) {

    # Get latest update to target table
    # We consider changes to dependency tables that are after this update time
    last_update <- tbl(conn, id(target_table)) %>%
      summarize(COALESCE(max(pmax(from_ts, until_ts, na.rm = T), na.rm = T), TO_TIMESTAMP("1900-01-01 09:00:00", "YYYY-MM-DD HH24:MI::SS"))) %>%
      pull() %>%
      as.character()

    # If more than one days is between last update and slice_ts, run the script for each of those days
    # This way, every change to age_groups occurs on the correct dates
    birthday_keys <- \(last_update) get_table(conn, "mg.epicpr_c_status", slice_ts = last_update) %>% # Valid at given date
      inner_join(active_cpr_status(last_update), by = "c_status") %>%
      filter(birth %in% !!reduce(map(age_cuts, ~ as.Date(last_update) - years(.)), c)) %>%
      distinct(cprnr)

    if (last_update != as.POSIXct("1900-01-01 09:00:00", tz = "UTC")) {
      days_missing <- as.numeric(difftime(as.Date(slice_ts), as.Date(last_update), units = "days")) - 1 # Barf...
      if (days_missing > 0) {
        for (d in seq(days_missing)) {
          ts_d <- as.POSIXct(last_update, tz = "UTC") + days(d)

          # Get subset of persons with birthday on ts_d
          subset <- get_table(conn, target_table, slice_ts = ts_d) %>%
            inner_join(birthday_keys(ts_d), by = "cprnr")

          # Close the currently valid records
          close <- subset %>%
            replace_na(list(valid_until = as.Date(ts_d)))

          # Determine new rows
          age_groups <- data.frame(birth = sort(reduce(map(age_cuts, ~ as.Date(ts_d) - years(.)), c), decreasing = T), age = age_cuts) %>%
            copy_to(conn, ., "rassky_tmp", overwrite = T) %>%
            mutate(age_group_2  = !!aggregate_age_sql(age, age_cuts = age_cuts_2),
                   age_group_9  = !!aggregate_age_sql(age, age_cuts = age_cuts_9),
                   age_group_10 = !!aggregate_age_sql(age, age_cuts = age_cuts_10)) %>%
            select(-age)

          # Open new rows for alive persons
          open <- subset %>%
            filter(is.na(valid_until)) %>%
            mutate(valid_from = as.Date(ts_d)) %>%
            select(-starts_with("age_group")) %>%
            left_join(age_groups, by = "birth")

          union_all(close, open) %>%
            update_snapshot(conn, target_table, as.character(ts_d), filters = birthday_keys(ts_d), tic = tic)

          tic <- Sys.time()
        }
      }
    }

    if (!full_mode) {
      # Load dependencies and determine the keys with changes
      # If last_update is from today, use <= operator, else use < operator since we get all updates in one lump
      f <- \(last_update, col) paste0("'", as.character(last_update), "' ", ifelse(as.Date(last_update) == today(), "<=", "<"), ' "', col, '"')

      # Look changes to the linelist (new records gets a delta of 1, removed records a delta of -1)
      changed_keys <- dependencies %>%
        map(~ {
          tmp <- tbl(conn, id(.)) %>%
            filter(
              (sql(!!f(last_update, "from_ts"))  && from_ts  <= slice_ts) | # Key is added
              (sql(!!f(last_update, "until_ts")) && until_ts <= slice_ts)   # Key is removed
            )
          if ("v_pnr" %in% colnames(tmp)) tmp <- rename(tmp, cprnr = v_pnr)

          tmp |>
              distinct(cprnr)
          }) |>
        reduce(union)

      # If persons are in changed_keys, there have been updates to their address or c_status
      # We also need to determine changes to age_groups
      changed_keys <- union(birthday_keys(slice_ts), changed_keys) %>% compute()
    }
  }
}




# Compute table corresponding to the slice_ts
{

  # Get birth and c_status information from mg.epicpr_c_status
  epicpr_c_status <- get_table(conn, "mg.epicpr_c_status", slice_ts = slice_ts) %>%
    rename(c_status_start = valid_from, c_status_end = valid_until)


  # Get the current epicpr table
  cpr_address <- get_table(conn, "mg.epicpr_adresse", slice_ts = slice_ts) %>%
    select(cprnr = v_pnr,
           municipality_id = c_kom,
           parish_id = c_mynkod,
           moved_in_date  = d_tilflyt_dato,
           moved_out_date = d_fraflyt_dato) %>%
    distinct() # TODO: This should only be a temporary measure

  # Consider only the filter subset
  cpr_address <- cpr_address %>% filter_keys(changed_keys)

  # Mapping region, province and municipality ##
  geographic <- get_table(conn, "prod.municipalities", slice_ts = slice_ts) %>%
    select(municipality_id, province, region) %>%
    mutate(province = str_remove(province, "^Landsdel "),
           region   = str_remove(region,   "^Region "))

  epicpr_address <- cpr_address %>%
    left_join(geographic, by = "municipality_id")

  # Check for zero-day change of address
  epicpr_address <- epicpr_address %>%
    filter(is.na(moved_out_date) | !(moved_in_date == moved_out_date)) %>%
    compute()




  # Add age group information to epicpr
  tmp <- epicpr_c_status %>%
    inner_join(active_cpr_status(slice_ts), by = "c_status") %>%
    select(cprnr, birth, c_status_start, c_status_end )

  age_group <- tmp %>%
    mutate(age = 0,
           valid_from = birth,
           valid_until = as.Date(as.Date(birth) + years(!!age_cuts[1])))

  for (i in seq_along(age_cuts)) {
    tt <- tmp %>%
      mutate(age = !!age_cuts[i],
             valid_from  = as.Date(as.Date(birth) + years(!!age_cuts[i])))
    if (i < length(age_cuts)) {
      tt <- tt %>% mutate(valid_until = as.Date(as.Date(birth) + years(!!age_cuts[i+1])))
    }

    age_group <- union_all(age_group, tt)
  }

  # Filter out age_groups that are not yet valid (validity in the future)
  age_group <- age_group %>%
    filter(valid_from <  c_status_end | is.na(c_status_end)) %>%    # Must be alive at time of age_group change
    filter(valid_from <= slice_ts) %>%                              # Age_group change must not must not be in the future
    mutate(valid_until = case_when(
      is.na(valid_until) & !is.na(c_status_end) ~ c_status_end,         # Record is open, but person is no longer alive - truncate valid_until date
      !is.na(c_status_end) & valid_until > c_status_end ~ c_status_end, # Record is closed, but person is no longer alive - truncate valid_until date
      valid_until > slice_ts ~ NA,                                      # valid_until is in the future, keep as open record
      TRUE ~ valid_until)) %>%
    select(-starts_with("c_status")) %>%
    distinct()

  # Determine age_groups
  epicpr_age <- age_group %>%
    mutate(age_group_2  = !!aggregate_age_sql(age, age_cuts = age_cuts_2),
           age_group_9  = !!aggregate_age_sql(age, age_cuts = age_cuts_9),
           age_group_10 = !!aggregate_age_sql(age, age_cuts = age_cuts_10)) %>%
    select(-age) %>%
    compute()



  # Combine the data sources
  epicpr <- interlace_sql(list(epicpr_c_status, epicpr_address, epicpr_age),
                          by = "cprnr",
                          colnames = c(t1.from = "c_status_start", t1.until = "c_status_end",
                                       t2.from = "moved_in_date",  t2.until = "moved_out_date")) %>%
    relocate(starts_with("valid"), .after = last_col())

}

# Push updates to the target_table
epicpr %>%
  update_snapshot(conn, target_table, slice_ts, filters = changed_keys, tic = tic)



# Add documentation to the target_table (note: some columns will be auto-documented)
docs <- tibble(column_name = character(), comment = character()) %>%
  add_row(comment = paste(sep = "<br>",
                         "This table combines age, address, and 'c_status' information for all individuals registred in the CPR registry.<br>",
                         "This information is constrained to a time period the information is valid (defined by the 'valid_from' and 'valid_until' colums)",
                         "These records can readily be merged onto other temporal data to provide the context information about the indivual at the given time.")) #%>%
  #add_row(column_name = "cprnr",           comment = NA) %>% # Auto
  #add_row(column_name = "birth",           comment = NA) %>% # Auto
  #add_row(column_name = "c_status",        comment = NA) %>% # Auto
  #add_row(column_name = "municipality_id", comment = NA) %>% # Auto
  #add_row(column_name = "parish_id",       comment = NA) %>% # Auto
  #add_row(column_name = "province",        comment = NA) %>% # Auto
  #add_row(column_name = "region",          comment = NA) %>% # Auto
  #add_row(column_name = "age_group_2",     comment = NA) %>% # Auto
  #add_row(column_name = "age_group_9",     comment = NA) %>% # Auto
  #add_row(column_name = "age_group_10",    comment = NA) %>% # Auto
  #add_row(column_name = "valid_from",      comment = NA) %>% # Auto
  #add_row(column_name = "valid_until",     comment = NA) # Auto

# Check for template comments
auto_docable <- docs %>% left_join(get_table(conn, "docs.templates") %>% collect(), suffix = c(".", ".template"), by = "column_name") %>% filter(!is.na(comment.template))
if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning("column ", ..1, " in table ", target_table, " can be auto-commented with: ", ..3))

# Commit to DB
log <- capture.output(
  pmap(docs, ~ db_comment(conn, target_table, column = ..1, comment = ..2, timestamp = slice_ts)))
failed <- log[!is.na(str_extract(log, "(WARNING|ERROR)"))]
if (length(failed) > 0) print(failed)
auto_comment(conn, target_table, timestamp = slice_ts)


close_connection(conn)
