#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=00:45:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : prod.covid_19_maalgruppe

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(purrr)
})

# Get the input_args from the environment
target_table <- Sys.getenv("TARGET_TABLE", unset = 'mg.vaccine_history')
slice_ts     <- Sys.getenv("TIMESTAMP",    unset = 'missing')
full_mode    <- any(c("--full") %in% commandArgs(trailingOnly = TRUE))

if (slice_ts == 'missing') stop()

conn <- get_connection()

# Check if there are any changes to the input data, and determine which CPR numbers are affected
{
  dependencies <- c('prod.covid_vaccination', 'prod.covid_19_maalgruppe')

  changed_keys <- NULL
  if (!full_mode && mg::table_exists(conn, target_table)){

    # Get latest update to target table
    # We consider changes to dependency tables that are after this update time
    last_update <- tbl(conn, in_schema(target_table)) %>%
      summarize(COALESCE(max(pmax(from_ts, until_ts, na.rm = T), na.rm = T), TO_TIMESTAMP('1900-01-01 09:00:00', 'YYYY-MM-DD HH24:MI::SS'))) %>%
      pull() %>%
      as.character()

    # Load dependencies and determine the keys with changes
    # If last_update is from today, use <= operator, else use < operator since we get all updates in one lump
    f <- \(last_update, col)paste0("'", as.character(last_update), "' ", ifelse(as.Date(last_update) == today(), '<=', '<'), ' "', col, '"')

    # Look changes to the linelist (new records gets a delta of 1, removed records a delta of -1)
    changed_keys <- dependencies %>%
      map(~ tbl(conn, in_schema(.)) %>%
            filter(
              (sql(!!f(last_update, 'from_ts'))  && from_ts  <= slice_ts) | # Key is added
              (sql(!!f(last_update, 'until_ts')) && until_ts <= slice_ts)   # Key is removed
            ) %>%
            select(cprnr = any_of(c('cprnr10', 'cpr', 'cprnr'))) %>%
            distinct()) %>%
      reduce(union)

    if (length(dependencies) > 1) changed_keys <- compute(changed_keys)
  }
}


{
  # Pull from DB
  vaccine_history <- get_table(conn, 'prod.covid_vaccination', slice_ts = slice_ts) %>%
    select(cprnr = cpr,
           vaccine_date = vaccinedate,
           vaccine_name = vaccinename) %>%
    filter_keys(changed_keys)

  # Arrange by vaccine_date to get name
  vaccine_history <- vaccine_history %>%
    window_order(vaccine_date) %>%
    group_by(cprnr) %>%
    mutate(vaccine_dose = row_number()) %>%
    ungroup()

  # Splitting target_group_id and target_group with regex
  target_group <- get_table(conn, 'prod.covid_19_maalgruppe', slice_ts = slice_ts) %>%
    select(cprnr = cpr, target_group = ny_maalgruppe_kombi) %>%
    distinct() %>%
    mutate(target_group_id = regexp_replace(target_group, '(\\..*)|(^\\D.*)', ''), # Everything after a period, or everything if line begins with a non-digit
           target_group    = str_remove(    target_group, '^\\d\\S*.\\s')) %>%  # group id gets removed
    mutate(target_group_id = case_when(
      target_group_id == '' ~ NA,
      TRUE ~ target_group_id))

  # Add to the data
  vaccine_history <- left_join(vaccine_history, target_group, by = 'cprnr')

  # Get all unique vaccine names
  vaccine_map <- vaccine_history %>%
    select(vaccine_name) %>%
    distinct() %>%
    filter(!is.na(vaccine_name))

  # Map to the vaccine producer
  vaccine_map <- vaccine_map %>%
    mutate(vaccine_producer = case_when(
      vaccine_name %like% '%Moderna%'     ~ 'Moderna',
      vaccine_name %like% '%Pfizer%'      ~ 'Pfizer BioNTech',
      vaccine_name %like% '%Comirnaty%'   ~ 'Pfizer BioNTech',
      vaccine_name %like% '%AstraZeneca%' ~ 'AstraZeneca',
      vaccine_name %like% '%Janssen%'     ~ 'Janssen',
      TRUE ~ vaccine_name))

  # Add to the data
  vaccine_history <- left_join(vaccine_history, vaccine_map, by = 'vaccine_name')

  # Compute the effective date for the vaccine
  vaccine_history <- vaccine_history %>%
    mutate(vaccine_effective_date = as.Date(case_when(
      vaccine_dose <= 2 ~ vaccine_date + days(14),
      TRUE              ~ vaccine_date + days(7))))


  # Label the vaccine effective dates
  vaccine_history <- vaccine_history %>%
    mutate(vaccine_status = case_when(
      vaccine_dose <= 2 ~ paste(vaccine_dose, 'stik + 14d'),
      vaccine_dose >= 3 ~ paste(vaccine_dose, 'stik + 7d')))

  # Determine when the dose is valid
  vaccine_history <- vaccine_history %>%
    group_by(cprnr) %>%
    window_order(vaccine_effective_date) %>%
    mutate(valid_until = lead(vaccine_effective_date)) %>%
    rename(valid_from = vaccine_effective_date) %>%
    ungroup() %>%
    select(cprnr, starts_with('target'), vaccine_dose, starts_with('vaccine'), everything()) %>%
    relocate(starts_with('valid'), .after = last_col())
}

# Push updates to the target_table
vaccine_history %>%
  update_snapshot(conn, target_table, slice_ts, filters = changed_keys, tic = tic)



# Add documentation to the target_table (note: some columns will be auto-documented)
docs <- tibble(column_name = character(), comment = character()) %>%
  add_row(comment = paste(sep = '<br>',
                          'This dataset contains a long-format record of vaccination data.',
                          'These are pulled from prod.covid_vaccination and enriched with target_group information from prod.covid_19_maalgruppe',
                          'The long format resembles the DDV format and will continueally grow over time as new doses are given',
                          'The table includes at least one record for each vaccinated person, and has a record for every vaccine dose given to the individual<br>',
                          'These records includes the "primary" vaccination group (target_group and target_group_id) of the person.',
                          'Note that the information on the primary vaccination groups is the latest available and in principle can change backwards in time.<br>',
                          'For the vaccinated persons, we include, for each dose, informaton on <br>1) the date of vaccination,<br>2) the name of the vaccine,<br>3) the producer of the vaccine.',
                          'In addition, we create a human readable label for easy identification of the vaccination status',
                          'The labels follows the semantic: "n stik + xd": (atleast) x days after n\'th dose.',
                          'The label is valid from the "valid_from" date up until (but not including) the "valid_until" date')) %>%
  #add_row(column_name = 'cprnr',             comment = NA) %>% # Auto
  add_row(column_name = 'target_group',      comment = 'The primary (covid-19) vaccination group from the initial vaccine rollout') %>%
  #add_row(column_name = 'target_group_id',   comment = 'The ID of the "primary" vaccination group') %>% # Auto
  add_row(column_name = 'vaccine_dose',      comment = 'Denotes which dose the information relates to') %>%
  add_row(column_name = 'vaccine_date',      comment = 'Denotes the date the dose was administered') %>%
  add_row(column_name = 'vaccine_name',      comment = 'The name of the vaccine dose') %>%
  add_row(column_name = 'vaccine_producer',  comment = 'The company producing the vaccine dose (interpreted from vaccine_name)') # %>%
#add_row(column_name = 'vaccine_status',    comment = 'A human-readable label for vaccination status') #%>% # Auto
#add_row(column_name = 'valid_from',        comment = NA) %>% # Auto
#add_row(column_name = 'valid_until',       comment = NA) # Auto

# Check for template comments
auto_docable <- docs %>% left_join(get_table(conn, 'docs.templates') %>% collect(), suffix = c('.', '.template'), by = 'column_name') %>% filter(!is.na(comment.template))
if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning('column ', ..1, ' in table ', target_table, ' can be auto-commented with: ', ..3))

# Commit to DB
log <- capture.output(
  pmap(docs, ~ db_comment(conn, target_table, column = ..1, comment = ..2, timestamp = slice_ts)))
failed <- log[!is.na(str_extract(log, "(WARNING|ERROR)"))]
if (length(failed) > 0) print(failed)
auto_comment(conn, target_table, timestamp = slice_ts)

close_connection(conn)
