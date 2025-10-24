#!/usr/bin/env Rscript

#PBS -l mem=1g
#PBS -l walltime=02:00:00
#PBS -l nodes=1:ppn=2
#PBS -d /ngc/projects/ssi_mg/rassky/logs
#PBS -j oe

# Dependencies : prod.variant_covid_19 prod.covid_19_wgs prod.basis_samples mg.lineage_info

tic <- Sys.time()
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(purrr)
})

# Get the input_args from the environment
target_table <- Sys.getenv("TARGET_TABLE", unset = "mg.miba")
slice_ts     <- Sys.getenv("TIMESTAMP",    unset = "missing")
full_mode    <- any(c("--full") %in% commandArgs(trailingOnly = TRUE))

if (slice_ts == "missing") stop()

# Whitelist of lineages that cannot be mapped
unmapable_lineages <- c("B.1.1.229", "B.1.88.1")

conn <- get_connection()

# For mg.miba, we have the extra strict requirement that pangolin data must be updated (to check for unmapped sequences)
wgs <- get_table(conn, "prod.covid_19_wgs", slice_ts = slice_ts) %>%
  filter(!is.na(lineage) && lineage != "None") %>%
  count(lineage)
pangolin <- get_table(conn, "mg.lineage_info", slice_ts = slice_ts)

stopifnot( "Unmapped WGS lineages detected. Update mg.lineage_info before running."={nrow(filter(left_join(wgs, pangolin, by = "lineage"), is.na(full))) == 0})


# Check if there are any changes to the input data, and determine which CPR numbers are affected
{
  dependencies <- c("prod.variant_covid_19", "prod.covid_19_wgs" ,"prod.basis_samples")

  changed_keys <- NULL
  if (!full_mode && table_exists(conn, target_table)){

    # Get latest update to target table
    # We consider changes to dependency tables that are after this update time
    last_update <- tbl(conn, id(target_table)) %>%
      summarize(COALESCE(max(pmax(from_ts, until_ts, na.rm = T), na.rm = T), TO_TIMESTAMP("1900-01-01 09:00:00", "YYYY-MM-DD HH24:MI::SS"))) %>%
      pull() %>%
      as.character()

    # Load dependencies and determine the keys with changes
    # If last_update is from today, use <= operator, else use < operator since we get all updates in one lump
    f <- \(last_update, col)paste0("'", as.character(last_update), "' ", ifelse(as.Date(last_update) == today(), "<=", "<"), ' "', col, '"')

    # Look changes to the linelist (new records gets a delta of 1, removed records a delta of -1)
    changed_keys <- dependencies %>%
      map(~ tbl(conn, id(.)) %>%
            filter(
              (sql(!!f(last_update, "from_ts"))  && from_ts  <= slice_ts) | # Key is added
              (sql(!!f(last_update, "until_ts")) && until_ts <= slice_ts)   # Key is removed
            ) %>%
            select(cprnr = any_of(c("cprnr10", "cpr", "cprnr"))) %>%
            distinct()) %>%
      reduce(union)

    if (length(dependencies) > 1) changed_keys <- compute(changed_keys)
  }
}



{
  ## Processing variant PCR (vpcr) information ##
  vpcr <- get_table(conn, "prod.variant_covid_19", slice_ts = slice_ts) %>%
    rename(cprnr = cprnr10,
           prdate = prdate_adjusted) %>%
    mutate(prdate = as.Date(prdate)) %>%
    filter(prdate < "2022-03-01") %>%
    filter_keys(changed_keys)

  # Removing multiple on same date
  vpcr <- vpcr %>%
    mutate(suspected_omicron = ifelse(suspected_omicron == 9, -9, suspected_omicron)) %>%
    group_by(cprnr, prdate) %>%
    slice_max(suspected_omicron + confirmed_omicron, with_ties = FALSE) %>%
    ungroup()

  # Set the variant (delta and omicron)
  vpcr <- vpcr %>%
    mutate(variant = case_when(
      suspected_omicron == -9 ~ "inkonklusiv",
      suspected_omicron ==  1 ~ "omicron",
      confirmed_omicron ==  1 ~ "omicron",
      TRUE ~ "delta")) %>%
    select(-suspected_omicron, -confirmed_omicron) %>%
    compute()


  ## Processing WGS (vpcr) information ##
  wgs <- get_table(conn, "prod.covid_19_wgs", slice_ts = slice_ts) %>%
    filter(sequence_status != "not_sequenced") %>%
    select(cprnr = cpr, prdate = date_sampling, sequence_status, lineage, variant = who_variant) %>%
    filter_keys(changed_keys)

  # Manually update the variant for inconclusive results
  wgs <- wgs %>%
    mutate(variant = case_when(
      sequence_status == "failed_qc" ~ "inkonklusiv",
      TRUE ~ tolower(variant)))



  ## Combine variant information sources results ##
  variant_information <- union(wgs %>%
                                 select(-sequence_status) %>%
                                 mutate(variant_source = "wgs"),
                               vpcr %>%
                                 mutate(variant_source = "vpcr"))

  # For each test, determine quality of the variant information
  variant_information <- variant_information %>%
    mutate(variant_quality = case_when( # Lower is better
      variant_source == "wgs"  & !is.na(lineage) ~ 0, # WGS has lineage
      variant_source == "wgs"  & (variant != "inkonklusiv" | is.na(variant)) ~ 0, # WGS has conclusive variant
      variant_source == "vpcr" &  variant != "inkonklusiv" ~ 1, # vpcr has conclusive variant
      variant_source == "wgs"  &  variant == "inkonklusiv" ~ 2, # WGS has inconclusive variant
      variant_source == "vpcr" &  variant == "inkonklusiv" ~ 3, # vpcr has inconclusive variant
      TRUE ~ NA))


  # Get lineage mapping function
  pangolin_data <- get_table(conn, "mg.lineage_info", slice_ts = slice_ts)

  # Check that all lineages are accounted for
  unmapped_samples <- variant_information %>%
    filter(!(is.na(lineage) | lineage == "None")) %>%
    filter(! lineage %in% unmapable_lineages) %>%
    select(lineage) %>%
    left_join(pangolin_data, by = "lineage") %>%
    filter(is.na(full)) %>%
    count(lineage) %>%
    as_tibble()

  if (nrow(unmapped_samples > 0)) {
    pmap(unmapped_samples, ~ warning(sprintf("%3d", ..2), " sample", ifelse(..2 > 1, "s", " "), " of lineage ", ..1, " not mapped"))
    stop("Stopping due to non-whitelisted unmapped lineages")
  }





  # Define custom lineages of interest
  variant_major <- c("BA.1", "BA.2", "BA.5")


  focus_lineage_map <- function(pangolin_data, variant_major = c("BA.2", "BA.5")) { # nocov start

    # Check arguments
    assert_data_like(pangolin_data)
    checkmate::assert_character(variant_major)

    focus_lineages <- pangolin_data |>
      dplyr::filter(.data$lineage %in% variant_major) |>
      dplyr::transmute(focus_lineage = .data$lineage, .data$full)

    lin_map <- pangolin_data |>
      dplyr::left_join(focus_lineages,                                              # "||" is a concatenate operator
                       sql_on = '"LHS"."full" = "RHS"."full" OR "LHS"."full" SIMILAR TO ("RHS"."full" || \'.%\')') |>
      dplyr::group_by(.data$lineage) |>
      dplyr::slice_max(focus_lineage) |>
      dplyr::ungroup() |>
      dplyr::select("lineage", "focus_lineage", "variant")

    return(lin_map)
  }


  # Add map to variant information and define the "focus_lineage" column "variant: sub-variant"
  lin_map <- focus_lineage_map(pangolin_data, variant_major) %>%
    unite("focus_lineage", "variant", "focus_lineage", sep = ": ", remove = F)

  # Add ": other" to variants that are subdivided
  other <- lin_map %>%
    distinct(variant, focus_lineage) %>%
    group_by(variant) %>%
    filter(n() > 1) %>%
    slice_min(focus_lineage)

  lin_map <- lin_map %>%
    inner_join(other, by = c("focus_lineage", "variant")) %>%
    mutate(focus_lineage = paste0(.data$variant, ": other")) %>%
    rows_update(lin_map, ., by = "lineage", unmatched = "ignore") %>%
   select("lineage", "focus_lineage")

  # Add focus lineage to variant information
  variant_information <- variant_information %>%
    left_join(lin_map, by = "lineage") %>%
    compute()


  # Pull from DB
  miba <- get_table(conn, "prod.basis_samples", slice_ts = slice_ts) %>%
    filter(as.Date(prdate) <= as.Date(slice_ts)) %>%
    filter_keys(changed_keys)

  # Removing duplicate samples of the same type on the same day. Giving preferences to conclusive tests and positive tests
  miba <- miba %>%
    group_by(cprnr, as.Date(prdate), casedef) %>%
    slice_min(resultat) %>% # First chose lowest "resultat"
    slice_min(prdate, with_ties = F) %>% # Then break ties by time of day test was taken
    ungroup() %>%
    transmute(cprnr, prdate = as.Date(prdate), casedef, resultat, avd) %>%
    compute()


  ## Removing repeated positive PCR tests within 60 days ##
  min_gap = 60

  # Determine positive PCR tests
  positive <- miba %>%
    filter(resultat == 1, casedef == "SARS2")

  # Add variant information and determine best available variant information per test
  positive <- positive %>%
    left_join(variant_information, by = c("cprnr", "prdate")) %>%
    group_by(cprnr, prdate, variant_quality) %>%
    summarise(variant        = max(variant, na.rm = TRUE), #
              lineage        = max(lineage, na.rm = TRUE),
              focus_lineage  = max(focus_lineage, na.rm = TRUE),
              variant_source = max(variant_source, na.rm = TRUE),
              .groups = "drop_last") %>%
    slice_min(variant_quality, with_ties = FALSE) %>%  # Lower is better
    ungroup() %>%
    compute()


  # First we detect tests which have no other tests in the preceding min_gap days
  # Join positives with positives if a previous test overlaps within min_gap
  first_positive <- positive %>%
    left_join(positive,
              suffix = c("", ".p"),
              sql_on = paste0('"LHS"."cprnr" = "RHS"."cprnr" AND
                               "LHS"."prdate" > "RHS"."prdate" AND
                               "LHS"."prdate" <= "RHS"."prdate" + ', min_gap))

  # Joins only happen, when a previous test overlaps, so filter out these rows
  first_positive <- first_positive %>%
    filter(is.na(prdate.p)) %>%
    select(-ends_with(".p")) %>%
    compute()


  # Not all will be caught this way, so we need to iteratively catch the rest
  # For each positive, merge on a known first positive within min_gap window
  tmp <- positive %>%
    left_join(first_positive,
              suffix = c("", ".p"),
              sql_on = paste0('"LHS"."cprnr" = "RHS"."cprnr" AND
                               "LHS"."prdate" >= "RHS"."prdate" AND
                               "LHS"."prdate" <= "RHS"."prdate" + ', min_gap)) %>%
    compute()

  possible_first_positive <- tmp %>% filter(is.na(prdate.p)) %>% # Those which lie more than min_gap days after a known first positive
    union(tmp %>% filter(variant != "inkonklusiv" & variant.p != "inkonklusiv" & variant != variant.p)) %>% # Those with a variant change
    union(tmp %>% filter(!starts_with(lineage, lineage.p))) %>% # Those with a lineage change (lineages have to diverge to be counted as a new infection) # TODO: Use "full" from pangolin (this is an edgecase)
    select(-ends_with(".p")) %>%
    compute()

  # Iterate
  k <- 0
  while(pull(count(possible_first_positive)) > 0 && k < 5) {

    # Choose the earliest of these possible tests and add to first_positives
    first_positive <- possible_first_positive %>%
      group_by(cprnr) %>%
      slice_min(prdate) %>%
      ungroup() %>%
      union(first_positive) %>%
      compute()

    # Check for more unaccounted for tests
    tmp <- positive %>%
      left_join(first_positive,
                suffix = c("", ".p"),
                sql_on = paste0('"LHS"."cprnr" = "RHS"."cprnr" AND
                                 "LHS"."prdate" >= "RHS"."prdate" AND
                                 "LHS"."prdate" <= "RHS"."prdate" + ', min_gap)) %>%
      compute()

    possible_first_positive <- tmp %>% filter(is.na(prdate.p)) %>%
      union(tmp %>% filter(variant != "inkonklusiv" & variant.p != "inkonklusiv" & variant != variant.p)) %>%
      union(tmp %>% filter(!starts_with(lineage, lineage.p))) %>%
      select(-ends_with(".p")) %>%
      compute()

    k <- k + 1
  }

  # Join first_positives to all test to so each test is matched to the last known positive
  labelled_pcr_tests <- miba %>%
    filter(casedef == "SARS2") %>%
    left_join(first_positive,
              suffix = c("", ".p"),
              sql_on = paste0('"LHS"."cprnr" = "RHS"."cprnr" AND
                               "LHS"."prdate" >= "RHS"."prdate"')) %>%
    group_by(cprnr, prdate) %>%
    slice_max(prdate.p) %>%
    ungroup()


  # Determine new infections and infection_id from the joined data
  labelled_pcr_tests <- labelled_pcr_tests %>%
    mutate(new_infection = case_when(
      prdate == prdate.p ~ 1,            # If the test starts an infection window, it has new_infection = 1
      prdate <= prdate.p + days(min_gap) ~ 0, # If a test lie within min_gap days, it has new infection = 0
      TRUE ~ NA)) %>% # If the test lie outside an infection window, it has NA as new_infection
    group_by(cprnr) %>%
    window_order(prdate) %>%
    mutate(infection_id = cumsum(new_infection)) %>%
    ungroup() %>%
    window_order() %>%
    select(-ends_with(".p")) %>%
    compute()


  # An infection may have several variant information of same quality
  # We break this tie using max (preferring variants later in alphabet)
  # Then, we for each infection_id, choose the highest quality variant information
  infection_id_variant_information <- labelled_pcr_tests %>%
    filter(resultat == 1) %>%
    group_by(cprnr, infection_id, resultat, variant_quality) %>%
    summarise(variant        = max(variant, na.rm = TRUE), #
              lineage        = max(lineage, na.rm = TRUE),
              focus_lineage  = max(focus_lineage, na.rm = TRUE),
              variant_source = max(variant_source, na.rm = TRUE),
              .groups = "drop_last") %>%
    slice_min(variant_quality, with_ties = FALSE) %>%  # Lower is better
    select(-variant_quality)


  # Merge onto test data
  miba_FU <- labelled_pcr_tests %>%
    select(cprnr, prdate, casedef, resultat, new_infection, infection_id) %>%
    left_join(infection_id_variant_information, by = c("cprnr", "infection_id", "resultat")) %>%
    right_join(miba, by = c("cprnr", "prdate", "casedef", "resultat"))
}


# Push updates to the target_table
miba_FU %>%
  update_snapshot(conn, target_table, slice_ts, filters = changed_keys, tic = tic)



# Add documentation to the target_table (note: some columns will be auto-documented)
docs <- tibble(column_name = character(), comment = character()) %>%
  add_row(comment = paste(sep = "<br>",
   "This dataset contains a interpreted / filtered version of prod.basis_samples.",
   "In this dataset, the samples are filtered such that only one test per day is counted per person.",
   "Preference is given, in order to: 1) positive result, 2) negative result, 3) inconclusive result (lowest value of prod.basis_samples.resultat is chosen).",
   "If several test at given preference exists, the earliest test on the day is chosen.<br>",
   'Positve tests are grouped into "infections" labelled by a consequtive "infection_id" for each person.',
   'An "infection" is started by a positive test which is given the value "new_infection = 1".<br>All tests that lie within a 60 day window of this test are given a "new_infection" value of 0.<br>Tests that lie after the 60 day window are have NA as "new_infection".',
   'If a positive test lies more than 60 days after the first test in the window (i.e. the window origin), the test defines a new "infection_id" and a corresponding new 60 day window.<br>The column "infection_id" tracks the number of infections the person has had up until the sampledate.<br>',
   'The positive samples are combined with variant information from wgs data (prod.covid_19_wgs) and variant PCR (prod.variant_covid_19)',
   'If multiple variant information is available per sample, preferences is given, in order, to:',
   '1) conclusive wgs result, 2) conclusive variant pcr result, 3) inconclusive wgs result, 4) inconclusive variant pcr result.',
   'If there is a change of variant or lineage within an infection window, the test with a new variant defines a new "infection" and starts a new 60 day window.',
   'Note that a "lineage refinement" where subsequent variant informations contains lower-level lineage information does not trigger a new infection.',
   'i.e. if one sample has lineage "AY.102" a subsequent lineage determination of "AY.102.2" is treated as same infection_id<br>',
   'The sample-level variant information are transfered to the infection_id-level using the identical preference for variant information.')) %>%
  #add_row(column_name = 'cprnr',           comment = NA) %>% # Auto
  #add_row(column_name = "prdate",          comment = NA) %>% # Auto
  add_row(column_name = 'new_infection',   comment = 'Is 1 if the test is the first positive PCR-test in the infection window.<br>Is 0 if the test is a follow-up positive PCR-test within the infection window.<br>Is NA if the test is negative OR an antigen test.') %>%
  add_row(column_name = 'infection_id',    comment = 'A cummulative counter assigned to tests.<br>The counter is a cumulative counter over new_infection and thus shows how many infections the person has had at the time of the test.') %>%
  #add_row(column_name = 'variant',         comment = NA) %>% # Auto
  #add_row(column_name = 'lineage',         comment = NA) %>% # Auto
  #add_row(column_name = 'focus_lineage',   comment = NA) %>% # Auto
  add_row(column_name = 'variant_source',  comment = 'Indicates the source of the variant information. Either "wgs" (whole-genome sequencing) or "vpcr" (variant-pcr).') #%>%
  #add_row(column_name = 'casedef',         comment = NA) %>% # Auto
  #add_row(column_name = 'resultat',        comment = NA) %>% # Auto
  #add_row(column_name = 'avd',             comment = NA) # Auto

# Check for template comments
auto_docable <- docs %>% left_join(get_table(conn, 'docs.templates') %>% collect(), suffix = c('.', '.template'), by = 'column_name') %>% filter(!is.na(comment.template))
if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning('column ', ..1, ' in table ', target_table, ' can be auto-commented with: ', ..3))

# Commit to DB
log <- capture.output(
  pmap(docs, ~ db_comment(conn, target_table, column = ..1, comment = ..2, timestamp = slice_ts)))
failed <- log[!is.na(str_extract(log, '(WARNING|ERROR)'))]
if (length(failed) > 0) print(failed)
auto_comment(conn, target_table, timestamp = slice_ts)

close_connection(conn)
