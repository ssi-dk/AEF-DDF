#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=02:00:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : prod.covid_19_patientlinelist mg.miba mg.epicpr mg.vaccine_history

unit_tests <- TRUE

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(purrr)
})

target_table <- Sys.getenv("TARGET_TABLE", unset = 'covid_data.regional_aggregation')
slice_time   <- Sys.getenv("TIMESTAMP",    unset = 'missing')

if (slice_time == 'missing') stop()

if (wday(slice_time, week_start=1) != 2) q() # Only run on Tuesdays

conn <- get_connection()


date_start <- as.Date("2020-03-01")
date_end <- as.Date(slice_time) - days(2)

all_dates <- copy_to(conn, tibble(date = seq.Date(from = date_start, to = date_end, by = 1)),
                     'regional_aggregations_tmp', overwrite = TRUE)


# Compute table corresponding to the slice_date
{
  ## epiCPR ##
  epicpr <- get_table(conn, 'mg.epicpr', slice_date = slice_time) %>%
    filter(date_start < valid_until | is.na(valid_until), valid_from <= date_end) %>%
    filter(c_status %in% c('01', '03'))

  ## Vaccine history ##
  vaccine_history <- get_table(conn, 'mg.vaccine_history', slice_date = slice_time) %>%
    filter(date_start < valid_until | is.na(valid_until), valid_from <= date_end)

  # Combine onto single time axes
  epicpr_vaccine <- interlace_sql(list(epicpr, vaccine_history), by = 'cprnr') %>%
    replace_na(list(vaccine_dose = 0, vaccine_status = 'Uvaccineret')) %>%
    compute()


  ## Merging epiCPR and vaccine history data to miba_data ##
  # Only look at tests that have changed since last update
  miba <- get_table(conn, 'mg.miba', slice_date = slice_time) %>%
    filter(casedef == 'SARS2', date_start <= prdate) %>%
    left_join(epicpr_vaccine, suffix = c('', '.p'),
              sql_on = '"LHS"."cprnr" = "RHS"."cprnr" AND
                      "LHS"."prdate" >= "RHS"."valid_from" AND
                      ("RHS"."valid_until" IS NULL OR "LHS"."prdate" < "RHS"."valid_until")') %>%
    compute()


  ## linelist ##
  linelist <- tbl(conn, in_schema('prod.covid_19_patientlinelist')) %>%
    filter(!is.na(newlyadmdate), from_ts <= slice_time) %>%
    transmute(cprnr = cpr,
              episodekey,
              prdate = sampledate,
              newlyadmdate,
              regadmission,
              covid = firstadmclassv2 %like% '% pga. covid-19',
              from_ts, until_ts)


  # Get all records for admission dates
  # Information can either change from linelist or from miba
  adm <- linelist %>%
    slice_time(slice_time) %>%
    filter(date_start <= newlyadmdate) %>%
    left_join(miba, by = c("cprnr", "prdate"))


  # Records for hospitalizations dates needs special treatment due to how they
  # are stored in the linelist
  hosp <- linelist %>%
    filter(regadmission == 'True') %>%
    inner_join(all_dates %>% inner_join(distinct(linelist, from_ts) %>% transmute(date = as.Date(from_ts)), by = 'date'),
               sql_on = '("RHS"."date" >= DATE("LHS"."from_ts") AND ("RHS"."date" < DATE("LHS"."until_ts") OR "LHS"."until_ts" IS NULL))') %>%
    left_join(miba, by = c("cprnr", "prdate")) %>%
    compute()

  # The covid classification is delayed, so we need to pull the newest available covid designation
  hosp <- linelist %>%
    group_by(cprnr, episodekey) %>%
    slice_max(from_ts) %>%  # use newest information
    ungroup() %>%
    select(cprnr, episodekey, covid) %>%
    right_join(hosp %>% select(-covid),
               by = c('cprnr', 'episodekey'))

  # ... but we need to remember the truncation
  last_classified_date <- linelist %>%
    filter(covid) %>%
    summarize(max(newlyadmdate, na.rm = T)) %>%
    pull()
  host <- hosp %>% mutate(covid = ifelse(date <= last_classified_date, covid, NA))



  #######################################
  # Aggregate testing data
  tmp_test <- miba %>%
    count(date = prdate, age_group_10, vaccine_status, region, name = 'n_test') %>%
    ungroup()

  tmp_pos <- miba %>%
    filter(resultat == 1) %>%
    count(date = prdate, age_group_10, vaccine_status, region, focus_lineage, name = 'n_positive') %>%
    ungroup()


  # Aggregate new admissions
  tmp_adm <- adm %>%
    count(date = newlyadmdate, region, age_group_10, vaccine_status, focus_lineage, covid, name = 'n_admission') %>%
    ungroup()

  # Aggregate hospitalized
  # 1) Find records with regadmission = 'True' (indicates they are hospitalized between from_ts and until_ts)
  # 2) Merge onto all_dates and count. As long as the record is valid, the person counts as a hospitalization
  tmp_hosp <- hosp %>%
    count(date, region, age_group_10, vaccine_status, focus_lineage, covid, name = 'n_hospital') %>%
    ungroup()

  # Determine the population in stratified groups
  group_vars <- c("vaccine_status", "age_group_10", "region")

  # Determine the starting population
  tmp_pop_start <- epicpr_vaccine %>%
    filter(valid_from <= date_start & (date_start < valid_until | is.na(valid_until))) %>%
    group_by_at(vars(!!group_vars)) %>%
    summarize(n_pop_start = n(), date = as.Date(date_start), .groups = 'drop')

  # Add the new_valid population
  tmp_pop_add <- epicpr_vaccine %>%
    filter(date_start < valid_from, # Starting pop is already accounted for, there for "<" and not "<="
           valid_from <= date_end) %>%
    group_by_at(vars(c(!!group_vars, "valid_from"))) %>%
    summarize(n_pop_add = n(), .groups = 'drop') %>%
    rename(date = valid_from)

  # Add the new_invalid population
  tmp_pop_remove <- epicpr_vaccine %>%
    filter(date_start  < valid_until,
           valid_until <= date_end) %>%
    group_by_at(vars(c(!!group_vars, "valid_until"))) %>%
    summarize(n_pop_remove = n(), .groups = 'drop') %>%
    rename(date = valid_until)


  # Get all combinations to merge onto
  all_combi <- epicpr_vaccine %>%
    distinct(vaccine_status, age_group_10, region) %>%
    full_join(all_dates, by = character())

  # Aggregate vaccine population
  tmp_pop <- all_combi %>%
    left_join(tmp_pop_start,  by = 'date', na_by = group_vars) %>%
    left_join(tmp_pop_add,    by = 'date', na_by = group_vars) %>%
    left_join(tmp_pop_remove, by = 'date', na_by = group_vars) %>%
    replace_na(list(n_pop_start = 0, n_pop_add = 0, n_pop_remove = 0)) %>%
    group_by(vaccine_status, age_group_10, region) %>%
    window_order(date) %>%
    mutate(n_population = cumsum(n_pop_start) + cumsum(n_pop_add) - cumsum(n_pop_remove)) %>%
    ungroup() %>%
    select(-n_pop_start, -n_pop_add, -n_pop_remove) %>%
    compute()

  # Some unit tests...
  stopifnot(nrow(tmp_pop %>% filter(n_population < 0)) == 0)
  stopifnot(nrow(tmp_pop %>% filter(!(vaccine_status == 'Uvaccineret'))) > 0)

  # We keep data sparse
  tmp_pop <- tmp_pop %>% filter(n_population > 0)

  # Combine population, test and hospital counts
  all_combi <- list(tmp_pop, tmp_test, tmp_pos, tmp_adm, tmp_hosp) %>%
    map( ~ select(., any_of(c('date', !!group_vars, 'focus_lineage', 'covid')))) %>%
    map( ~ distinct(.)) %>%
    reduce(union) %>%
    inner_join(all_dates, by = 'date')

  vac_test_pop_reg <- all_combi %>%
    left_join(tmp_pop,  by = 'date', na_by = group_vars) %>%
    left_join(tmp_test, by = 'date', na_by = group_vars) %>%
    left_join(tmp_pos,  by = 'date', na_by = c(group_vars, 'focus_lineage')) %>%
    left_join(tmp_adm,  by = 'date', na_by = c(group_vars, 'focus_lineage', 'covid')) %>%
    left_join(tmp_hosp, by = 'date', na_by = c(group_vars, 'focus_lineage', 'covid'))
}

# Push updates to the target_table
vac_test_pop_reg %>%
  update_snapshot(conn, target_table, slice_time, tic = tic)



# Write documentation for the target_table (note: some columns will be auto-documented)
docs <- tibble(column_name = character(), comment = character()) %>%
  add_row(comment = paste(sep = '<br>',
                          'This dataset contains regionally aggregated data designed for the covid models.',
                          'The aggregation occurs at the following levels:',
                          'date, age_group_10, vaccine_status, region, focus_lineage and covid (see definitions in the corresponding columns)',
                          'Important: Aggregations occur at various levels which leads to the information being "nested".',
                          'To understand what this means, consider the population counts (n_pop) which are aggregated by date, age_group_10, vaccine_status and region.',
                          'Contrast this with data like hospitalizations (n_admission, n_hospital) which are aggregated by additional levels (focus_lineage and covid).',
                          'Population counts are repeated across all data at these finer aggregations.',
                          'Therefore, you must take care when summarizing across the dataset:',
                          'If you are interested in summarizing population counts, you need to only use the first record at the level of aggregation. If not, you will count data multiple times.<br>',
                          'The levels of aggregation are as follows:',
                          'n_population:\t date, age_group_10, vaccine_status, region',
                          'n_test:\t date, age_group_10, vaccine_status, region',
                          'n_positive:\t date, age_group_10, vaccine_status, region, focus_lineage',
                          'n_admission:\t date, age_group_10, vaccine_status, region, focus_lineage, covid',
                          'n_hospital:\t date, age_group_10, vaccine_status, region, focus_lineage, covid')) %>%
  add_row(column_name = 'date',         comment = 'Date the aggregation covers') %>%
  add_row(column_name = 'covid',        comment = 'Is TRUE if firstadmclassv2 matches the regular expression "* pga. covid-19"') %>%
  add_row(column_name = 'n_population', comment = 'Population size') %>%
  add_row(column_name = 'n_test',       comment = 'Number of tests performed') %>%
  add_row(column_name = 'n_positive',   comment = 'Number of positive among tested') %>%
  add_row(column_name = 'n_admission',  comment = 'Number of admissions with a positive covid-19 test') %>%
  add_row(column_name = 'n_hospital',   comment = 'Number of people hospitalized with a positive covid-19 test')

# Check for template comments
auto_docable <- docs %>% left_join(get_table(conn, 'docs.templates') %>% collect(), suffix = c('.', '.template'), by = 'column_name') %>% filter(!is.na(comment.template))
if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning('column ', ..1, ' in table ', target_table, ' can be auto-commented with: ', ..3))

# Commit to DB
log <- capture.output(
  pmap(docs, ~ db_comment(conn, target_table, column = ..1, comment = ..2, timestamp = slice_time)))
failed <- log[!is.na(str_extract(log, '(WARNING|ERROR)'))]
if (length(failed) > 0) print(failed)
mg::auto_comment(conn, target_table, timestamp = slice_time)

close_connection(conn)
