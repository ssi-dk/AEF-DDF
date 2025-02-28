tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(stringr)
})

target_table <- 'mg.dmi'

conn <- get_connection()

#' This script updates the dmi data
#' Since we do not get automatic transfers of the dmi data, this script is special.
#' The script will look for files added to /ngc/projects/common/datasets/dmi
#' (the folder also contains a script you can copy locally to generate new files)
#' If a new files is found in the folder at the time of the execute, it uses the newest file.
#' This means that the date of the file is NOT the primary time-keeping variable as we
#' use for the other tables. Normally, the date of the data-file is used to set the from_ts
#' and until_ts variables and thus work as the effective time-keeping variables.
#' In this instance, the dmi data will always be delayed in coming to the
#' NGC platform. We will use the logs to determine from_ts / until_ts variables
#' based on WHEN the data was added to NGC (instead of when the data was available on the ftpserver)

#' (Note that the first time build, DOES us the date of the file to set the from_ts / until_ts
#' variables since we can, and it is still the "cleanest" way of doing when we initialize)



# Get list of data files
path <- '/ngc/projects/ssi_mg/common/datasets/dmi/'
prognosis_path <- paste0(path, 'prognosis/')
prognosis_files <- dir(prognosis_path)
prognosis_files <- prognosis_files[str_detect(prognosis_files, 'dk_ext_[0-9]{10}.csv.gz')]
prognosis_dates <- as.Date(str_extract(prognosis_files, '[0-9]{8}(?=[0-9]{2})'), format = '%Y%m%d')
prognosis_files <- prognosis_files[order(prognosis_dates)]

history_path <- paste0(path, 'historical_records/')
history_files <- dir(history_path)
history_files <- history_files[str_detect(history_files, 'dmi_data_[0-9]{4}-[0-9]{2}-[0-9]{2}.rds')]
history_dates <- as.Date(str_extract(history_files, '[0-9]{4}-[0-9]{2}-[0-9]{2}'))
history_files <- history_files[order(history_dates)]

# Determine the combination of files
available_dmi_files <- tibble(date = unique(c(history_dates, prognosis_dates))) %>% # using unique instead of union to avoid random type casting for R
  dplyr::left_join(tibble(date = history_dates,   history_file   = history_files),   by = join_by(closest(date >= date)), suffix = c('', '.history')) %>%
  dplyr::left_join(tibble(date = prognosis_dates, prognosis_file = prognosis_files), by = join_by(closest(date >= date)), suffix = c('', '.prognosis')) %>%
  arrange(date)

# Get a list of the logs
logs <- tbl(conn, in_schema("prod.logs")) %>%
  replace_na(list(schema = 'prod')) %>%
  unite('t', schema, 'table', sep = '.')

find_in_log <- function(log_content, str) {
  ss <- str_extract(log_content, str)  # Match string
  if (all(is.na(ss))) return(NA_character_)
  else return(log_content[!is.na(ss)])
}

# Load coordinates map
coordinates <- readRDS(paste0(path, 'dmi_municipality_coordinates.rds'))

# Define function that updates the tables
update_dmi_table <- function(dmi_files) {

  # Walk over files and push to the table (should work as long as purrr::pwalk runs the operations sequentially)
  dmi_files %>%
    arrange(date) %>%
    pwalk(~ {
      history   <- readRDS(paste0(history_path, ..2))
      prognosis <- data.table::fread(paste0(prognosis_path, ..3)) %>%
        rename_with(stringr::str_to_lower) %>%
        as_tibble()

      # Determine prognosis for each municipality
      municipality_map <- prognosis %>%
        distinct(latit, longi) %>%
        cross_join(coordinates) %>%
        group_by(municipality_id) %>%
        slice_min((longi-longitude)^2+(latit-latitude)^2) %>%
        ungroup() %>%
        select(-longitude, -latitude)

      # Pull main prognosis from the data
      prognosis <- prognosis %>%
        filter(mbr == 0) %>%
        right_join(municipality_map, by = c('latit', 'longi')) %>%
        transmute(date = as.Date(as.character(validate), format="%Y%m%d"),
                  municipality_id,
                  min_temp  = NA_real_,
                  mean_temp = t2m,
                  max_temp  = t2mx) %>%
        filter(date > ..1)

      # Truncate the history
      history <- history %>%
        filter(date <= ..1)

      # Combine before transfer
      dmi_data <- union_all(history   %>% mutate(source = 'historical record'),
                            prognosis %>% mutate(source = 'prognosis'))

      # Each date - municipality combination should only occur once
      stopifnot(nrow(dmi_data) == nrow(distinct(dmi_data, date, municipality_id )))

      copy_to(conn, dmi_data, name = "dmi_tmp", overwrite = T) %>%
        update_snapshot(conn, target_table, paste(..1, '09:00:00'), message = paste("data from", ..2, "and", ..3))
    })


  # Add documentation to the target_table (note: some columns will be auto-documented)
  docs <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains a combination of historical metrological observations from DMI and their',
                            'corresponding prognoses for these observations',
                            'The historical records are drawn from DMI climateData api whereas the prognoses are provided',
                            'directly from DMI via a ftpserver.<br>',
                            'Variable "min_temp" corresponds to "min_temp" in the climateData and has no prognosis',
                            'Variable "mean_temp" corresponds to "mean_temp" in the climateData and "t2m" in prognosis data',
                            'Variable "max_temp" corresponds to "mean_temp_w_date" in the climateData and "t2mx" in prognosis data')) %>%
    add_row(column_name = 'date',      comment = 'Date of the DMI observations') %>%
    add_row(column_name = 'min_temp',  comment = 'Minimum temperature (°C)') %>%
    add_row(column_name = 'mean_temp', comment = 'Mean temperature (°C)') %>%
    add_row(column_name = 'max_temp',  comment = 'Maximum temperature (°C)') %>%
    add_row(column_name = 'source',    comment = 'Either "historical record" or "prognosis"')

  # Check for template comments
  auto_docable <- docs %>% left_join(get_table(conn, 'docs.templates') %>% collect(), suffix = c('.', '.template'), by = 'column_name') %>% filter(!is.na(comment.template))
  if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning('column ', ..1, ' in table ', target_table, ' can be auto-commented with: ', ..3))

  # Commit to DB
  log <- capture.output(
    pmap(docs, ~ db_comment(conn, target_table, column = ..1, comment = ..2, timestamp = glue::glue("{min(dmi_files$date)} 09:00:00"))))
  failed <- log[!is.na(str_extract(log, '(WARNING|ERROR)'))]
  if (length(failed) > 0) print(failed)
  mg::auto_comment(conn, target_table, timestamp = glue::glue("{min(dmi_files$date)} 09:00:00"))
}


# First time build:
if (!mg::table_exists(conn, target_table)) { # The script is not called by qsub_updates.sh

  # We compile the update dates for the dmi data based on all files in the historical records and progosis folders
  available_dmi_files %>%
    dplyr::mutate(across(.cols = 'history_file', # We only have one history file, so we use this back in time
                         .fns = \(x)zoo::na.locf(x))) %>%
    dplyr::select(date, history_file, prognosis_file) %>%
    update_dmi_table()

} else {

  #' If the table already exists, we check to see if
  #' there is a new file. If yes, add the newer file to the current dmi data

  # Look through logs for which dmi files have been used
  last_used_files <- logs %>%
    filter(t == target_table) %>%
    slice_max(date) %>%
    pull(log_file) %>%
    paste0('/ngc/projects/ssi_mg/logs/', .) %>%
    readLines() %>% # Read from the log file
    find_in_log('Message') %>%
    str_extract_all('(dmi_data_[0-9]{4}-[0-9]{2}-[0-9]{2}.rds)|(dk_ext_[0-9]{10}.csv.gz)') %>%
    list_c()
  last_used_files <- tibble(history_file = pluck(last_used_files, 1), prognosis_file = pluck(last_used_files, 2)) %>%
    transmute(date.history   = as.Date(str_extract(history_file,   '[0-9]{4}-[0-9]{2}-[0-9]{2}')),
              date.prognosis = as.Date(str_extract(prognosis_file, '[0-9]{8}(?=[0-9]{2})'), format = '%Y%m%d'))

  # Check for new files is available in the folder
  newest_avilable_history_file   <- available_dmi_files %>% slice_max(date.history,   with_ties = F) %>% select(contains('history'))
  newest_avilable_prognosis_file <- available_dmi_files %>% slice_max(date.prognosis, with_ties = F) %>% select(contains('prognosis'))

  # Load new files if found
  if ((newest_avilable_history_file$date.history     > max(last_used_files$date.history,   na.rm = T)) ||
      (newest_avilable_prognosis_file$date.prognosis > max(last_used_files$date.prognosis, na.rm = T))) {

    tibble(date = lubridate::today(),
           history_file   = newest_avilable_history_file$history_file,
           prognosis_file = newest_avilable_prognosis_file$prognosis_file) %>%
    update_dmi_table()
  }
}

close_connection(conn)
