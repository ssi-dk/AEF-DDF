#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=02:00:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : prod.covid_19_wgs

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(stringr)
})

target_table <- Sys.getenv("TARGET_TABLE", unset = 'mg.mutation_info')
slice_ts     <- Sys.getenv("TIMESTAMP",    unset = 'missing')

conn <- get_connection()

#' This script updates the mutation info
#' Since we do not get automatic transfers of the pango-designation data, this
#' script is special.
#' The script will look for files added to /ngc/projects/common/datasets/mutation_info
#' (the folder also contains a script you can copy locally to generate new files)
#' If a new files is found in the folder at the time of the execute, use the newest file
#' This means that the date of the file is NOT the primary time-keeping variable as we
#' use for the other tables. Normally, the date of the data-file is used to set the from_ts
#' and until_ts variables and thus work as the effective time-keeping variables.
#' In this instance, the mutation data will always be delayed in coming to the
#' NGC platform. This means that there will be several days worth of automatic
#' data-updates that will use "old" data since it cannot know that new data is
#' available. Instead, we will use the logs to determine from_ts / until_ts variables
#' based on WHEN the data was added to NGC (instead of when the data was available on github)

#' (Note that the first time build, DOES us the date of the file to set the from_ts / until_ts
#' variables since we can, and it is still the "cleanest" way of doing when we initialize)

# Some lineages cannot be mapped. Here is a whitelist to remove them from consideration
whitelist <- c('B.1.1.229', 'B.1.88.1')

# Get list of data files
mut_path <- '/ngc/projects/ssi_mg/common/datasets/mutation_info/'
mut_files <- dir(mut_path)
mut_files <- mut_files[str_detect(mut_files, 'mutations_[0-9]{4}-[0-9]{2}-[0-9]{2}.RData')]
mut_dates <- as.Date(str_extract(mut_files, '[0-9]{4}-[0-9]{2}-[0-9]{2}'))

# Look for existing files
mut_files <- mut_files[order(mut_dates)]

# Get a list of the logs
logs <- tbl(conn, in_schema("prod.logs")) %>%
  replace_na(list(schema = 'prod')) %>%
  unite('t', schema, 'table', sep = '.')

find_in_log <- function(log_content, str) {
  ss <- str_extract(log_content, str)  # Match string
  if (all(is.na(ss))) return(NA_character_)
  else return(log_content[!is.na(ss)])
}


# First time build:
if (slice_ts == 'missing') { # The script is not called by qsub_updates.sh

  # Determine the update dates for the wgs data
  # (We want a corresponding update of the mutation_info data to trigger
  # automatic updates for other scripts. That way, we can use this script
  # to monitor unmapped lineages -- and stop the chain of updates if there
  # are any unmapped lineages)

  # Add the list of files to a temporary table
  mut_files_db <- tibble(mut_date = mut_dates, mut_file = mut_files) %>%
    copy_to(conn, ., name = 'rassky_tmp', overwrite  = T)

  wgs_update_dates <- logs %>%
    filter(t == 'prod.covid_19_wgs') %>%
    distinct(wgs_date = as.Date(date)) %>%
    union(select(mut_files_db, wgs_date = mut_date))  # Manually add the mut_files before this date

  # Determine the corresponding mutation_info files for the wgs_data
  mut_file_map <- mut_files_db %>%
    right_join(wgs_update_dates, sql_on = '"LHS"."mut_date" <= "RHS"."wgs_date"') %>%
    group_by(wgs_date) %>%
    slice_max(mut_date) %>%
    as_tibble()

  # Initialize the table (should work as long as purrr::pmap runs the operations sequentially)
  mut_file_map %>%
    filter(!is.na(mut_date)) %>%
    pwalk(~ {
      load(paste0(mut_path, ..2))

      copy_to(conn, mutations, name = "mut_use_tmp", overwrite = T) %>%
        update_snapshot(conn, target_table, paste(..3, '09:00:00'), message = paste("data from", ..2))
    })


  # Check if these files can map all known lineages
  lineage_info <- get_table(conn, 'mg.lineage_info')

  lineage_detection <- get_table(conn, 'prod.covid_19_wgs') %>%
    filter(!is.na(lineage) && lineage != 'None') %>%
    count(lineage, name = 'n_detections') %>%
    left_join(lineage_info, by = 'lineage')


  mapping_efficiency <- get_table(conn, target_table) %>%
    left_join(lineage_info, by = 'lineage') %>%
    full_join(lineage_detection, by = 'full') %>%
    select(lineage_gisaid = lineage.x, lineage_wgs = lineage.y, full, n_detections, mutation)

  mapping_efficiency %>%
    group_by(full) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    count(is.na(mutation))

  mapping_efficiency <- tbl(conn, in_schema(target_table)) %>%
    full_join(lineage_detection, by = 'lineage') %>%
    filter(!is.na(wgs_date))

  unmapped_linages <- mapping_efficiency %>%
    #filter(!(lineage %in% whitelist)) %>%
    filter(is.na(full)) %>%
    group_by(lineage) %>%
    summarize(first_wgs_date = min(wgs_date, na.rm = T),
              last_wgs_date  = max(wgs_date, na.rm = T),
              n = sum(n, na.rm = T))
  if(pull(count(unmapped_linages)) > 0) {
    warning('Some lineages could not be mapped')
    print(unmapped_linages)
  }

  late_mappings <- mapping_efficiency %>%
    group_by(wgs_date, lineage) %>%
    mutate(map_available = as.Date(from_ts) <= wgs_date && (is.na(until_ts) | wgs_date < as.Date(until_ts))) %>%
    slice_max(map_available) %>%
    filter(!map_available) %>%
    summarize(first_wgs_date = min(wgs_date, na.rm = T),
              last_wgs_date  = max(wgs_date, na.rm = T),
              first_pango_date = as.Date(min(from_ts,  na.rm = T)),
              n = sum(n, na.rm = T), .groups = 'drop')
  if(pull(count(late_mappings)) > 0) {
    warning('Some lineages are mapped in a later update of pango-designation')
    print(late_mappings)
  }


} else {

  #' If the table already exists, we check to see if
  #' 1) there is a new file. If yes, add the newer file to the current pangolin designation
  #' 2) Then, check if we can map the current sequences. If not, give an error and do not update
  #' If yes, then upload the newer data

  # Look through logs for which lin_use files have been used
  date_of_newest_used_file <- logs %>% filter(t == target_table) %>% pull(log_file) %>%
    map(~ str_split(read_file(paste0('/ngc/projects/ssi_mg/logs/', .)), '\n')[[1]]) %>%  # Read from the log file
    map(~ find_in_log(., 'Message')) %>%
    map(~ str_extract(., '(?<=lin_use_)(\\S*)(?=.RData$)')) %>%
    reduce(union) %>%
    max(as.Date(.), na.rm = T) # Somehow, this does not work inside a reduce? wtf?

  # New files is available in the folder
  if (max(lin_dates) > date_of_newest_used_file) {
    load(paste0(lin_path, lin_files[which.max(lin_dates)]))
    lin_map <- copy_to(conn, lin_use, name = "lin_use_tmp", overwrite = T)
    lin_map_date <- max(lin_dates)
  } else {
    lin_map <- get_table(conn, target_table, slice_ts = slice_ts)
    lin_map_date <- date_of_newest_used_file
  }

  # Check if these files can map all known lineages
  lineage_detection <- get_table(conn, 'prod.covid_19_wgs', slice_ts = slice_ts) %>%
    filter(!is.na(lineage) && lineage != 'None') %>%
    count(lineage)

  # Check if we can map sequences
  mapping_efficiency <- union(tbl(conn, in_schema(target_table)), lin_map) %>%
    right_join(lineage_detection, by = 'lineage')

  unmapped_linages <- mapping_efficiency %>%
    filter(!(lineage %in% whitelist)) %>%
    filter(is.na(full))

  if(pull(count(unmapped_linages)) > 0) {
    print(unmapped_linages %>% select(lineage, n))
    stop('Some lineages could not be mapped')
    close_connection(conn)
    quit(status=1)
  }

  # If we make it this far, all lineages have been mapped
  lin_map %>%
      update_snapshot(conn, target_table, slice_ts, tic = tic, message = paste("data from", paste0('lin_use_', lin_map_date, '.RData')))
}


close_connection(conn)
