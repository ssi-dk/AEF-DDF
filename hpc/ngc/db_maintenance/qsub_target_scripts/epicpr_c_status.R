#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=01:15:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : prod.cpr3_t_person

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(purrr)
})

# Get the input_args from the environment
target_table <- Sys.getenv("TARGET_TABLE", unset = "mg.epicpr_c_status")
slice_ts     <- Sys.getenv("TIMESTAMP",    unset = "missing")
full_mode    <- any(c("--full") %in% commandArgs(trailingOnly = TRUE))

if (slice_ts == "missing") stop()

conn <- get_connection()

# Check if there are any changes to the input data, and determine which CPR numbers are affected
{
  dependencies <- c("prod.cpr3_t_person")

  changed_keys <- NULL
  if (!full_mode && mg::table_exists(conn, target_table)) {

    # Get latest update to target table
    # We consider changes to dependency tables that are after this update time
    last_update <- tbl(conn, in_schema(target_table)) %>%
      summarize(COALESCE(max(pmax(from_ts, until_ts, na.rm = T), na.rm = T), TO_TIMESTAMP("1900-01-01 09:00:00", "YYYY-MM-DD HH24:MI::SS"))) %>%
      pull() %>%
      as.character()

    # Load dependencies and determine the keys with changes
    # If last_update is from today, use <= operator, else use < operator since we get all updates in one lump
    f <- \(last_update, col)paste0("'", as.character(last_update), "' ", ifelse(as.Date(last_update) == today(), "<=", "<"), ' "', col, '"')

    # Look changes to the linelist (new records gets a delta of 1, removed records a delta of -1)
    changed_keys <- dependencies %>%
      map(~ tbl(conn, in_schema(.)) %>%
            filter(
              (sql(!!f(last_update, "from_ts"))  && from_ts  <= slice_ts) | # Key is added
              (sql(!!f(last_update, "until_ts")) && until_ts <= slice_ts)   # Key is removed
            ) %>%
            select(cprnr = v_pnr) %>%
            distinct()) %>%
      reduce(union)

    if (length(dependencies) > 1) changed_keys <- compute(changed_keys)
  }
}




# Compute table corresponding to the slice_ts
{

  # Get birth and c_status information from cpr3_t_person
  cpr_c_status <- get_table(conn, "prod.cpr3_t_person", slice_ts = slice_ts) %>%
    select(cprnr = v_pnr,
           birth = d_foddato,
           c_status,
           valid_from = d_status_hen_start) %>%
    distinct() # TODO: This should only be a temporary measure

  # Consider only the filter subset
  cpr_c_status <- cpr_c_status %>% filter_keys(changed_keys)

  # Those with changes get assigned "01" from birth
  epicpr_c_status <- cpr_c_status %>%
    filter(birth < valid_from) %>%  # if valid_from is not na, there is a change in status
    mutate(c_status = "01",
           valid_from = birth) %>%
    union(cpr_c_status)

  # Determine changes to the c_status
  epicpr_c_status <- epicpr_c_status %>%
    mutate(valid_from = case_when(
      is.na(valid_from) ~ birth,
      TRUE ~ valid_from)) %>%
    window_order(valid_from) %>%
    group_by(cprnr) %>%
    mutate(valid_until = lead(valid_from)) %>%
    ungroup()

  # Do not include persons without a birth date
  epicpr_c_status <- epicpr_c_status %>%
    filter(!is.na(birth))
}

# Push updates to the target_table
epicpr_c_status %>%
  update_snapshot(conn, target_table, slice_ts, filters = changed_keys, tic = tic)



# Add documentation to the target_table (note: some columns will be auto-documented)
docs <- tibble(column_name = character(), comment = character()) %>%
  add_row(comment = paste(sep = "<br>",
                          "This table combines expands the c_statues from table cpr3_t_person for all individuals registered in the CPR registry.<br>",
                          "The table differs from cpr3_t_person by assuming a c_status = '01' for individuals up until other codes are registered",
                          "The c_status information is constrained to a time period the information is valid (defined by the 'valid_from' and 'valid_until' column)",
                          "These records can readily be merged onto other temporal data to provide the context information about the individual at the given time.")) #%>%
#add_row(column_name = "cprnr",           comment = NA) %>% # Auto
#add_row(column_name = "birth",           comment = NA) %>% # Auto
#add_row(column_name = "c_status",        comment = NA) %>% # Auto
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
