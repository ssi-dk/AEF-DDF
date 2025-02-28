#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
})

conn <- get_connection()

output_dir <- '/ngc/projects/ssi_mg/common/db_maintenance/db_rapports/'

# To determine rapports that need to be updated
# we first find the timestamp of the each rapport
last_rapport_update <- tibble(file = dir(output_dir)) %>%
  mutate(db_table = str_extract(file, '.*(?=\\.html)')) %>%
  filter(str_detect(db_table, '.*\\..*')) %>%
  mutate(last_rapport_update = file.info(paste0(output_dir, file))$mtime) %>%
  select(db_table, last_rapport_update)

last_db_update <- get_table(conn, 'prod.logs') %>%
  replace_na(list(schema = 'prod')) %>%
  unite('db_table', 'schema', 'table', sep = '.') %>%
  group_by(db_table) %>%
  mutate(last_db_update = COALESCE(end_time, start_time)) %>%
  slice_max(last_db_update) %>%
  select(db_table, last_db_update) %>%
  collect()

# Then we easily find the tables that have been updated
db_table <- full_join(last_rapport_update, last_db_update, by = 'db_table') %>%
  filter(is.na(last_rapport_update) | last_db_update > last_rapport_update) %>%
  pull(db_table)

# Create new rapports
generate_db_rapport(conn, db_table, output_dir)

close_connection(conn)
