#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(mg)
  library(stringr)
  library(readr)
  library(data.table)
})

con <- get_connection()
target_table <- tbl(con, DBI::Id(schema = "prod", table = "logs"))

force_mode <- any(c("-f", "--force", "--full") %in% commandArgs(trailingOnly = TRUE))

path <- '/ngc/projects/ssi_mg/logs/'
log_files <- dir(path)

if (!force_mode) {
  # We parse files not in the db AND unfinished files that are changed after latest end_time
  db_logs <- target_table %>%
    select(log_file, end_time, success) %>%
    collect()

  new_logs     <- log_files[log_files %notin% pull(db_logs, 'log_file')]
  changed_logs <- db_logs %>% filter(!success) %>%
    left_join(data.frame(log_file = log_files, mtime = file.info(paste0(path, log_files))$mtime), by = 'log_file') %>%
    filter(difftime(mtime, end_time, units = 'secs') > 1) %>%
    pull(log_file)

  log_files <- union(new_logs, changed_logs)
}

if (length(log_files) == 0 && !interactive()) q()

find_in_log <- function(log_content, str) {

  # Match string
  ss <- str_extract(log_content, str)

  if (all(is.na(ss))) {
    return(NA_character_)
  } else {
    return(log_content[!is.na(ss)])
  }
}


parse_log_file <- function(log_file) {

  # Read from the log file
  log_content <- read_file(paste0(path, log_file))
  log_content <- str_split(log_content, '\n')[[1]]

  # Get starting time, stopping time, and duration
  start_time <- as.POSIXct(str_split(log_content[1], ' - ')[[1]][1])
  end_time <- as.POSIXct(file.info(paste0(path, log_file))$mtime)
  duration <- format(round(difftime(end_time, start_time), digits = 2))

  # Check for success
  success <- !is.na(find_in_log(log_content, 'Finished processing'))

  # Get the parsed file
  parsed_file <- str_extract(find_in_log(log_content, '(Checking file)|(Parsing data)'), '(?<=\\s)(\\S*\\.\\S*)') # Preceded by whitespace, any number of non-whitespace, a period, any number of non-whitespace

  # Change mode based on input file type
  # "Checking file": Piotr's parsing of raw input
  # "Parsing data": Our parsing of raw tables
  if (!is.na(find_in_log(log_content, 'Checking file'))) {

    # Infer the table updated
    db_table <- str_to_lower(str_extract(parsed_file, '([a-zA-Z]\\S*)(?=_\\S*.\\S*)')) # Starts with character, followed by any number of non_whitespace, stop when "_*.*" patterns is found
    db_schema <- NA

    db_timestamp <- str_extract(log_file, '([0-9]{4}_[0-9]{2}_[0-9]{2})|([0-9]{2}[a-zA-Z]{3}[0-9]{4})') %>%
      parse_date_time(c("ymd", "dby"), tz = "Europe/Copenhagen", locale = "da_DK")
    hour(db_timestamp) <- 9

  } else if (!is.na(find_in_log(log_content, 'Parsing data'))) {
    db_schema <- str_to_lower(str_extract(parsed_file, '(\\w*)(?=.\\S*)')) # Everything until first period
    db_table  <- str_to_lower(str_extract(parsed_file, '(?<=[.])(\\S*)'))  # Everything after the first period

    db_timestamp <- str_extract(find_in_log(log_content, 'timestamp for table'), '([0-9-]+ [0-9:.]+)$') %>%
      parse_date_time(c('%Y-%m-%d %H:%M:%S', '%Y-%m-%d %H:%M:%OS'), tz = "Europe/Copenhagen")

  } else { # Something has gone wrong -- skip file
    return(NULL)
  }

  # Get update information
  n_deactivations <- as.numeric(str_extract(find_in_log(log_content, 'Deactivate'), '\\d*$')) # Last digits
  n_insertions    <- as.numeric(str_extract(find_in_log(log_content, 'Insert'),     '\\d*$')) # Last digits

  # Get message from log file
  message = str_extract(find_in_log(log_content, 'Message'), '(?<=Message: ).*')

  return(data.table(date = db_timestamp, schema = db_schema, table = db_table, n_insertions, n_deactivations, start_time, end_time, duration, success, message, log_file))
}

logs <- lapply(log_files, parse_log_file) %>% rbindlist()

if (nrow(logs) > 0){

  if (force_mode) {
    # Delete all rows and rebuild entire table
    invisible(DBI::dbExecute(con, paste("DELETE FROM", remote_name(target_table))))
  }

  logs_tmp <- copy_to(con, logs, name = "logs_tmp", overwrite = T)
  upserted <- rows_upsert(target_table, logs_tmp, by = "log_file", in_place = T)

  if (interactive()) cat(nrow(logs_tmp), "rows affected")
}
