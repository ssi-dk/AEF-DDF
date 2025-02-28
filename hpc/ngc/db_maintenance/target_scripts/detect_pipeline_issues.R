#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(mg)
  library(zoo)
  library(furrr)
})

conn <- get_connection()

# Check for updates to data:
db_updates <- get_table(conn, 'prod.logs') %>%
  replace_na(list(schema = 'prod')) %>%
  unite('db_table', 'schema', 'table', sep = '.') %>%
  group_by(db_table, as.Date(start_time)) %>%
  slice_max(success) %>%
  slice_max(n_insertions + n_deactivations, with_ties = F) %>%
  ungroup() %>%
  mutate(weekday = as.numeric(to_char(start_time, 'ID')),
         week = ceiling((as.Date(start_time) - !!floor_date(today(), 'week')) / 7)) %>%
  collect()


# Look for missing data (consider up to the last 28 dates with updates (must be within the previous 4 weeks))
baseline <- db_updates %>%
  group_by(db_table) %>%
  arrange(desc(start_time)) %>%
  filter(row_number() <= 28, !!today() - weeks(4) <= as.Date(start_time)) %>%
  group_by(db_table, weekday) %>%
  filter(n() > 1) %>%  # ignore single occurrences
  mutate(n_updates = n_insertions + n_deactivations) %>%
  slice_max(n_updates) %>%
  transmute(db_table, weekday, expect_updates = n_updates > 0) %>%
  arrange(db_table, weekday)


# Warnings should be issued when either
# (1) it has been more than 7 days since last update (unless the table has not been updated for a while)
# (2) 3 expected updates have not been done

expected_updates <- tibble(date = seq.Date(today() - days(14), today() - days(1), by = '1 day')) %>%
  mutate(weekday = wday(date, week_start = 1)) %>%
  left_join(baseline, by = 'weekday')

transfers <- db_updates %>%
  transmute(date = as.Date(start_time), db_table, success, updates = n_insertions + n_deactivations > 0) %>%
  group_by(db_table, date) %>%
  slice_max(success, with_ties = F) %>%
  ungroup() %>%
  right_join(expected_updates, by = c('date', 'db_table')) %>%
  mutate(latest_success = case_when(success ~ date, TRUE ~ NA_Date_)) %>%
  group_by(db_table) %>%
  arrange(date) %>%
  mutate(cum_latest_success = na.locf(latest_success, na.rm = F),
         latest_success = max(latest_success, na.rm = T)) %>%
  distinct()

# Detect 7 days lapses in transfers
week_since_transfer <- transfers %>%
  filter(success) %>%
  group_by(db_table) %>%
  slice_max(date) %>%
  filter(date <= today() - days(8))

# Detect 3 day lapses in transfers
missed_expected_transfers <- transfers %>%
  filter(latest_success == cum_latest_success) %>%
  count(db_table, latest_success) %>%
  filter(n >= 3)

# Detect 3 day lapses in updates
missed_expected_updates <- transfers %>%
  group_by(db_table) %>%
  arrange(date) %>%
  #filter(row_number() <= 3) %>%
  mutate(missing_updates = !updates & expect_updates) %>%
  mutate(id = ifelse(missing_updates, cumsum(missing_updates & !lag(missing_updates, 1)), NA)) %>% # assing temporary grouping id when updates are missing
  filter(!is.na(id)) %>%
  group_by(id, .add = T) %>%
  summarize(n = n(),
            latest_success = max(latest_success),
            .groups = 'drop_last') %>%
  slice_max(id) %>%
  select(-id) %>%
  filter(n >= 3)

# Create error message:
errors <- list(week_since_transfer %>% transmute(db_table, latest_success, error = 'No transfers during last 7 days'),
               missed_expected_transfers %>% transmute(db_table, latest_success, error = paste('Missed', n, 'expected transfers')),
               missed_expected_updates %>% transmute(db_table, latest_success, error = paste('Missed', n, 'expected updates'))) %>%
    reduce(union_all)

if (nrow(errors) > 0) {
  print(errors)
  sink(paste0('/ngc/projects/ssi_mg/rassky/logs/detect_pipeline_issues_', format(today(), '%m%d'), '.log'))
  printr('Error in pipeline detected')
  print(errors)
  sink()
}



close_connection(conn)
