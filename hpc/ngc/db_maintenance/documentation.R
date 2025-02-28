#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(mg)
})


# Warning, this script is surprisingly slow to run.


conn <- get_connection()

target_table <- 'docs.templates'

# Write the template documentation
# If comment has the form "@abc" the comment will be treated as an alias for the column "abc"
# If column "abc" does not exist, the comment reference will be ignored. Therefore, I use it to
# write my suggestions as to who should fill in the information :)
# Anything you put after the reference (seperated by a space) gets added after the reference is expanded
template_docs <- tibble(column_name = character(), comment = character())

# Administrative information columns
template_docs <- template_docs %>%
  add_row(column_name = 'cprnr',                comment = 'Personal identification number of the individual (anonymised)') %>%
  add_row(column_name = 'cpr',                  comment = '@cprnr') %>%
  add_row(column_name = 'v_pnr',                comment = '@cprnr') %>%
  add_row(column_name = 'cprnr10',              comment = '@cprnr') %>%
  add_row(column_name = 'personnummer',         comment = '@cprnr') %>%
  add_row(column_name = 'sex',                  comment = 'Biological sex of the person') %>%
  add_row(column_name = 'patient_koen',         comment = '@sex') %>%
  add_row(column_name = 'c_kon',                comment = '@sex') %>%
  add_row(column_name = 'birth',                comment = 'Birthdate for the person') %>%
  add_row(column_name = 'd_foddato',            comment = '@birth') %>%
  add_row(column_name = 'patient_foedselsdato', comment = '@birth') %>%
  add_row(column_name = 'age_group_2',          comment = 'Person age stratified into 0-59-year-olds and 60+-year-olds') %>%
  add_row(column_name = 'age_group_9',          comment = 'Person age stratified into 10-year age groups from 0-10, 11-19, ... 80+-year-olds') %>%
  add_row(column_name = 'age_group_10',         comment = 'Person age stratified into functional age groups: 0-4, 5-11, 12-19, 20-29, ... 80+-year-olds') %>%
  add_row(column_name = 'region',               comment = 'Region (NUTS 2 level)') %>%
  add_row(column_name = 'region_id',            comment = 'Region ID (NUTS 2 level)') %>%
  add_row(column_name = 'regionskode',          comment = '@region_id') %>%
  add_row(column_name = 'reg_regioncode',       comment = '@region_id') %>%
  add_row(column_name = 'province',             comment = 'Province (NUTS 3 level)') %>%
  add_row(column_name = 'province_id',          comment = 'Province ID (NUTS 3 level)') %>%
  add_row(column_name = 'municipality',         comment = 'Municipality (LAU 1 level)') %>%
  add_row(column_name = 'municipality_id',      comment = 'Municipality ID (LAU 1 level)') %>%
  add_row(column_name = 'c_kom',                comment = '@municipality_id') %>%
  add_row(column_name = 'municipalitycode',     comment = '@municipality_id') %>%
  add_row(column_name = 'parish_id',            comment = 'Parish ID (LAU 2 level)') %>%
  add_row(column_name = 'parish',               comment = '@parish_id') %>%
  add_row(column_name = 'parishcode',           comment = '@parish_id') %>%
  add_row(column_name = 'parishname',           comment = 'Parish name (LAU 2 level)') %>%
  add_row(column_name = 'zipcodecity',          comment = 'Zip code') %>%
  add_row(column_name = 'c_vej',                comment = 'Address road (anonymised)') %>%
  add_row(column_name = 'v_husnum',             comment = 'Adresss road number') %>%
  add_row(column_name = 'v_etage',              comment = 'Adresss floor') %>%
  add_row(column_name = 'v_sidedoer',           comment = 'Additional address specification') %>%
  add_row(column_name = 'v_ligeulige',          comment = '@margru') %>%
  add_row(column_name = 'c_mynnvn',             comment = '@margru') %>%
  add_row(column_name = 'c_mynkod',             comment = '@margru') %>%
  add_row(column_name = 'c_status',             comment = 'Status of the individual in the CPR registry') %>%
  #add_row(column_name = 'id',                   comment = '@margru') %>% ?
  add_row(column_name = 'd_tilflyt_dato',       comment = 'Date of moving to address') %>%
  add_row(column_name = 'd_fraflyt_dato',       comment = 'Date of moving from address') %>%
  add_row(column_name = 'd_tilflyt_kom_dato',   comment = 'Date of moving to municipality') %>%
  add_row(column_name = 'd_fraflyt_kom_dato',   comment = 'Date of moving from municipality')

# Covid related information columns
template_docs <- template_docs %>%
  add_row(column_name = 'prdate',               comment = 'Sample date for the covid-test') %>%
  add_row(column_name = 'sampledate',           comment = '@prdate') %>%
  add_row(column_name = 'date_sampling',        comment = '@prdate') %>%
  add_row(column_name = 'prdate_adjusted',      comment = '@prdate') %>%
  add_row(column_name = 'avd',                  comment = 'ID for the location/entity that handled the covid-test') %>%
  add_row(column_name = 'variant',              comment = 'WHO designation of the covid variant') %>%
  add_row(column_name = 'who_variant',          comment = '@variant') %>%
  add_row(column_name = 'lineage',              comment = 'Lineage of covid variants') %>%
  add_row(column_name = 'focus_lineage',        comment = 'Lineage of covid variants grouped at a functional level') %>%
  add_row(column_name = 'episodekey',           comment = 'Identifier in the patientlinelist that indicates the "episode". Episode 1 is any covid-test. Higher episode numbers inidicate a re-infection') %>%
  add_row(column_name = 'casedef',              comment = 'Indicates the type of test "SARS2" when test is a PCR-test. "SARSG" when test is an antigen-test') %>%
  add_row(column_name = 'resultat',             comment = 'The result of the covid-test:<br>1 = positive<br>2 = negative<br>9 = inconclusive<br>99 = pending<br>100 = irrelevant') %>%
  add_row(column_name = 'first_vaccinedate',    comment = 'Date of the first covid-19 vaccine') %>%
  add_row(column_name = 'second_vaccinedate',   comment = 'Date of the second covid-19 vaccine') %>%
  add_row(column_name = 'revacc1_vaccinedate',  comment = 'Date of the third covid-19 vaccine') %>%
  add_row(column_name = 'revacc2_vaccinedate',  comment = 'Date of the fourth covid-19 vaccine') %>%
  add_row(column_name = 'first_vaccinename',    comment = 'Name of the first covid-19 vaccine') %>%
  add_row(column_name = 'second_vaccinename',   comment = 'Name of the second covid-19 vaccine') %>%
  add_row(column_name = 'revacc1_vaccinename',  comment = 'Name of the third covid-19 vaccine') %>%
  add_row(column_name = 'revacc2_vaccinename',  comment = 'Name of the fourth covid-19 vaccine') %>%
  add_row(column_name = 'vaccine_status',       comment = 'A human-readable label for number of vaccine doses a person has at the given time') %>%
  add_row(column_name = 'target_group_id',      comment = 'The primary (covid-19) vaccination group the individual was placed in') %>%
  add_row(column_name = 'maalgruppe_id',        comment = '@target_group_id') %>%
  add_row(column_name = 'priority_maalgruppe',  comment = '@target_group_id')


# Database related columns
template_docs <- template_docs %>%
  add_row(column_name = 'schema',               comment = 'Name of schema in database') %>%
  add_row(column_name = 'table',                comment = 'Name of table in database') %>%
  add_row(column_name = 'valid_from',           comment = 'Indicates the starting date for the information in the record') %>%
  add_row(column_name = 'gaeldende_fra_dato',   comment = '@valid_from') %>%
  add_row(column_name = 'gyldigfradato',        comment = '@valid_from') %>%
  add_row(column_name = 'valid_until',          comment = 'Indicates the ending date for the information in the record (not including)') %>%
  add_row(column_name = 'gyldigtildato',        comment = '@valid_until') %>%
  add_row(column_name = 'from_ts',              comment = 'Indicates the data-date where record was added to the DB') %>%
  add_row(column_name = 'until_ts',             comment = 'Indicates the data-date where record was removed from the DB') %>%
  add_row(column_name = 'checksum',             comment = 'Checksum of all column values. Used for tracking the insertion/deactivation of records')

# LPR3 entries
template_docs <- template_docs %>%
  #add_row(column_name = 'contact_id',           comment = '@margru') %>%
  #add_row(column_name = 'kontakt_id',           comment = '@contact_id') %>%
  add_row(column_name = 'sluttidspunkt',        comment = '@margru') %>%
  add_row(column_name = 'starttidspunkt',       comment = '@margru') %>%
  add_row(column_name = 'ejerforhold',          comment = '@margru') %>%
  add_row(column_name = 'fejl',                 comment = '@margru') %>%
  add_row(column_name = 'kode',                 comment = '@margru') %>%
  add_row(column_name = 'occupation',           comment = 'Newest profession name ( in case of several profession, nurse and medical doctor) if the healthcare worker is less than 65 years of age at the sample date.') %>% # Take from DIAS covid_19_patientlinelist variable description
  add_row(column_name = 'speciality',           comment = '@margru') %>%
  add_row(column_name = 'diagnose_id',          comment = '@margru') %>%
  add_row(column_name = 'hovedspeciale',        comment = '@margru') %>%
  add_row(column_name = 'institutionsejer',     comment = '@margru') %>%
  add_row(column_name = 'sundhedsinstitution',  comment = '@margru')







# temp
template_docs <- template_docs %>%
  add_row(column_name = 'date',                 comment = '@local') %>%
  add_row(column_name = 'table_description',    comment = NA)

# Ensure uniqueness
template_docs %>% count(column_name) %>% filter(n > 1)
stopifnot('Column defined multiple times in template'={identical(select(template_docs, column_name), distinct(select(template_docs, column_name)))})

# Update the template database (Filter out invalid references)
template_docs %>%
  mutate(ref = ifelse(comment %like% '^@.', str_remove(comment, '^@'), NA)) %>%
  left_join(template_docs, suffix = c('', '.p'), by = c('ref' = 'column_name')) %>%
  filter(is.na(ref) | (!is.na(ref) & !is.na(comment.p))) %>%
  select(-ends_with('.p'), -ref) %>%
  copy_to(conn, ., name = in_schema(target_table), overwrite = T, temporary = FALSE)



# Look for columns which are candidates for auto commenting:
db_missing_comments  <- dplyr::tbl(conn, in_schema("information_schema", "columns")) %>%
  inner_join(get_tables(conn), by = c('table_schema'='schema', 'table_name'='table'), copy = T) %>%
  unite('db_table', 'table_schema', 'table_name', sep = '.') %>%
  mutate(comment = dbplyr::sql('col_description(CAST("db_table" AS "regclass"), "ordinal_position")')) %>%
  filter(is.na(comment)) %>%
  select(-comment) %>%
  left_join(template_docs, by = 'column_name', copy = T) %>%
  filter(is.na(comment))


db_missing_comments %>%
  count(column_name) %>%
  arrange(desc(n)) %>%
  compute() %>%
  left_join(db_missing_comments, by = 'column_name') %>%
  select(column_name, n, db_table) %>%
  print(n = 30)






# Documenting Piotr's tables

# Code that generates the code below
# tables <- get_tables(conn) %>%
#   filter(schema == 'prod') %>%
#   pull(table)
#
# tables %>%
#   map(~ tbl(conn, in_schema('prod', .)) %>% colnames()) %>%
#   walk2(tables, ~ printr("## prod.", .y,
#                         '\n{\n  docs$', .y, " <- tibble(column_name = character(), comment = character()) %>%\n    add_row(comment = paste(sep = '<br>',\n                            '@table_description')) %>%\n",
#                         paste(map2(.x, reduce(map(.x, str_length), max), ~ paste0("    add_row(column_name = '", .x, "',", paste0(rep('', .y - str_length(.x)), collapse = ''), " comment = NA)")), collapse = ' %>%\n'),
#                         '\n\n  check_docs(docs$', .y, ')\n}\n\n'))


docs <- list()


# This code checks for columns that can be automatically documented
check_docs <- \(x) left_join(x, template_docs, suffix = c('.', '.template'), by = 'column_name') %>%
  filter(!is.na(comment.template)) %>%
  pmap(~ paste('column', ..1, 'can be auto-commented')) %>% reduce(union, .init = '') %>% printr(sep = '\n')


## prod.basis_samples
{
  docs$basis_samples <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 63 - covid-19 basis_samples for modelgroup',
                            '',
                            'This report contains ALL covid-19 tests from MiBa - both PCR and antigentests.',
                            'NOTE: This means that each person can have several tests per day.',
                            'The filtered / interpreted table "mg.miba" might be more suitable for your needs.',
                            'However, use this table if you need the extra information that is not transfered to mg.miba')) %>%
    add_row(column_name = 'batchid',      comment = NA) %>%
    #add_row(column_name = 'casedef',      comment = NA) %>% # Auto
    #add_row(column_name = 'cprnr',        comment = NA) %>% # Auto
    add_row(column_name = 'indate',       comment = NA) %>%
    #add_row(column_name = 'prdate',       comment = NA) %>% # Auto
    add_row(column_name = 'qtnr',         comment = NA) %>%
    #add_row(column_name = 'avd',          comment = NA) %>% # Auto
    #add_row(column_name = 'resultat',     comment = NA) %>% # Auto
    add_row(column_name = 'isentinel',    comment = NA) %>%
    add_row(column_name = 'itestcenter',  comment = NA) %>%
    add_row(column_name = 'cprnr_fro',    comment = NA) %>%
    add_row(column_name = 'cprnr_grl2',   comment = NA) %>%
    add_row(column_name = 'utdate',       comment = NA) %>%
    add_row(column_name = 'extid',        comment = 'epiMIBA KMA internal sample code') %>%
    add_row(column_name = 'testpt_adhoc', comment = NA) %>%
    add_row(column_name = 'ct_value',     comment = NA) #%>%
    #add_row(column_name = 'checksum',     comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',      comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',     comment = NA)     # Auto

  check_docs(docs$basis_samples)
}


## prod.corona_spoergeskema_data
{
  docs$corona_spoergeskema_data <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 70 - covid-19 coronaproever.dk questionaire data for modelgroup',
                            '',
                            "This report contains coronaproever.dk's questionnaire data combined with basis_samples (corona tested data)",
                            'All data from questionnaire data from coronaproever.dk with testresult')) %>%
    #add_row(column_name = 'cpr',            comment = NA) %>% # Auto
    add_row(column_name = 'bookingcreated', comment = NA) %>%
    add_row(column_name = 'spoergeskema',   comment = NA) %>%
    add_row(column_name = 'answer',         comment = NA) %>%
    add_row(column_name = 'tested',         comment = NA) %>%
    #add_row(column_name = 'sampledate',     comment = NA) %>% # Auto
    add_row(column_name = 'samplerundate',  comment = NA) #%>%
    #add_row(column_name = 'checksum',       comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',        comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',       comment = NA)     # Auto

  check_docs(docs$corona_spoergeskema_data)}


## prod.covid_19_maalgruppe
{
  docs$covid_19_maalgruppe <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 61 - covid-19 målgruppedata for modelgroup',
                            '',
                            'This standard report contains information about malgruppe to the modelgroup on SSI.',
                            'All CPR numbers who are in the target group for the cvid.19 vaccination.')) %>%
    #add_row(column_name = 'cpr',                       comment = NA) %>% # Auto
    #add_row(column_name = 'sex',                       comment = NA) %>% # Auto
    add_row(column_name = 'vaccineage',                comment = NA) %>%
    add_row(column_name = 'vaccineagegrp',             comment = NA) %>%
    #add_row(column_name = 'first_vaccinedate',         comment = NA) %>% # Auto
    #add_row(column_name = 'first_vaccinename',         comment = NA) %>% # Auto
    add_row(column_name = 'faerdigvaccineret',         comment = NA) %>%
    #add_row(column_name = 'second_vaccinedate',        comment = NA) %>% # Auto
    #add_row(column_name = 'second_vaccinename',        comment = NA) %>% # Auto
    #add_row(column_name = 'revacc1_vaccinedate',       comment = NA) %>% # Auto
    #add_row(column_name = 'revacc1_vaccinename',       comment = NA) %>% # Auto
    add_row(column_name = 'reportage',                 comment = NA) %>%
    add_row(column_name = 'reportagegrp',              comment = NA) %>%
    add_row(column_name = 'rundate_fact',              comment = NA) %>%
    add_row(column_name = 'vacc_rundate',              comment = NA) %>%
    add_row(column_name = 'maalgrp4_underopdelt',      comment = NA) %>%
    add_row(column_name = 'ny_maalgruppe_kombi',       comment = NA) %>%
    add_row(column_name = 'persontype',                comment = NA) %>%
    add_row(column_name = 'template',                  comment = NA) %>%
    #add_row(column_name = 'priority_maalgruppe',       comment = NA) %>% # Auto
    add_row(column_name = 'c_kom_vacc',                comment = NA) %>%
    add_row(column_name = 'kommune_vacc',              comment = NA) %>%
    add_row(column_name = 'v_postnr_vacc',             comment = NA) %>%
    add_row(column_name = 'regionsnavn_vacc',          comment = NA) %>%
    add_row(column_name = 'c_kom_current',             comment = '@municipality_id - at time of data-transfer') %>%
    add_row(column_name = 'kommune_current',           comment = '@municipality - at time of data-transfer') %>%
    add_row(column_name = 'v_postnr_current',          comment = '@zipcodecity - at time of data-transfer') %>%
    add_row(column_name = 'regionsnavn_current',       comment = '@region - at time of data-transfer') %>%
    add_row(column_name = 'death30day_first_vaccine',  comment = NA) %>%
    add_row(column_name = 'death30day_second_vaccine', comment = NA) %>%
    add_row(column_name = 'db07_tekst',                comment = NA) %>%
    add_row(column_name = 'db07_kode',                 comment = NA) %>%
    #add_row(column_name = 'occupation',                comment = NA) %>% # Auto
    #add_row(column_name = 'speciality',                comment = NA) %>% # Auto
    add_row(column_name = 'arb_region',                comment = NA) %>%
    add_row(column_name = 'plejehjem_vaccine',         comment = NA) #%>%
    #add_row(column_name = 'checksum',                  comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',                   comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',                  comment = NA) %>% # Auto
    #add_row(column_name = 'revacc2_vaccinename',       comment = NA) %>% # Auto
    #add_row(column_name = 'revacc2_vaccinedate',       comment = NA) # Auto

  check_docs(docs$covid_19_maalgruppe)
}


## prod.covid_19_patientlinelist
{
  docs$covid_19_patientlinelist <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 60 - covid-19 Patientlineliste for modelgroup',
                            '',
                            'This standard report contains information from the patientlinelist for covid-19 to the modelgroup on SSI. patientlinelist.',
                            'Episodes for covid-19.',
                            'Only PCR tests is included. Excluded CPR that have address in Greenlanders and Faroese.',
                            '',
                            'The patientlinelist is structured around "episodes":',
                            'i) If a person has only ever tested negative, they have a single record in the linelist with their latest test',
                            'ii) The first time a person tests positive, they have a single record in the linelist with their positive test',
                            '-- any subsequent test within 60 days are ignored',
                            'iii) If a person tests positive again after these 60 days, they get a new record in the linelist with the new positive test',
                            'iv) This repeats for every positive tests a person takes that are more than 60 days from apart from the episode defining tests.',
                            '',
                            'The positve tests are coupled with hospitalizations if either:',
                            'a) the person is hospitalized in the 14 days following the test',
                            'b) the positive test was taken within 48 hours of hospitalization',
                            'If the hospitalization is coupled due to condition b), the date of hospitalization (newlyadmdate) is set to the date of the test')) %>%
    #add_row(column_name = 'cpr',                   comment = NA) %>% # Auto
    #add_row(column_name = 'episodekey',            comment = NA) %>% # Auto
    add_row(column_name = 'casedefinition',        comment = 'COVID-19 test result:<br>1 = Positive; 0 = negative; 9 = pending for result') %>%
    add_row(column_name = 'casedefinition_label',  comment = NA) %>%
    add_row(column_name = 'sampledate',            comment = 'Date of sample test:<br>If [Casedefinition] = 1 then Prdate from first positive test result; IF [Casedefinition] = 0 then Prdate for first negative test result ; first Prdate for Pending if [CaseDefinition] = 9') %>%
    add_row(column_name = 'covid19_status',        comment = 'Decides wether the patient is recovered, dead or infected:<br>0 = infected; 1 = recovered; 2 = dead') %>%
    add_row(column_name = 'covid19_enddate',       comment = 'Expected recovery date, or date of death if dead') %>%
    add_row(column_name = 'covid19_rule',          comment = 'Identification of rule recovery:<br>Rule 1 no admissions, recovery in 14days; Rule 2 short admissions, recovery in 14days; Rule 3 admissions longer than 14days after the sample date but shorter than 30 days; Rule 4 admissions longer than 30 days -> recovery within 30 days no matter what; Rule 5 ICU after 30 days then ICU_end = recovery date; Rule 6 ICU after 30 days and died in ICU then date of death = recovery date') %>%
    add_row(column_name = 'dateofdeath_final',     comment = 'CPR registry Date of Death') %>%
    add_row(column_name = 'death30days_final',     comment = 'To filter to have the dead within 30 days after the sample date:<br>CPR Registry; 1 = death within 30 days after the sample date') %>%
    #add_row(column_name = 'sex',                   comment = NA) %>% # Auto
    add_row(column_name = 'sampleage',             comment = 'Age at the date of the sample date') %>%
    #add_row(column_name = 'zipcodecity',           comment = NA) %>% # Auto
    #add_row(column_name = 'municipalitycode',      comment = NA) %>% # Auto
    #add_row(column_name = 'parishname',            comment = NA) %>% # Auto
    #add_row(column_name = 'parishcode',            comment = NA) %>% # Auto
    #add_row(column_name = 'region',                comment = NA) %>% # Auto
    add_row(column_name = 'kma',                   comment = 'First KMA name') %>%
    add_row(column_name = 'symptomsstartdate',     comment = 'Start date of the symptoms') %>%
    add_row(column_name = 'symptoms',              comment = 'Symptoms list:<br>Symptomer_txt_1 + Symptomer_txt_2 + Symptomer_txt_3 + Symptomer_txt_4') %>%
    add_row(column_name = 'pregnancy',             comment = 'If the person is pregnant or not:<br>Pregnancy=1') %>%
    #add_row(column_name = 'occupation',            comment = NA) %>% # Auto
    #add_row(column_name = 'speciality',            comment = NA) %>% # Auto
    add_row(column_name = 'placeofinfection_en',   comment = NA) %>%
    add_row(column_name = 'epilprfirstadm',        comment = 'First admission within 14 days after your sample date and longer than 12 hours:<br>First admission date in Epilpr (more than 12 hours long within 14 days after the sample date) = 1 ; If no admissions more than 12 hours long after within 14 days after the sample date = 0') %>%
    add_row(column_name = 'epilprfirst_in',        comment = 'First admission date in LPR within 14 days after sample date and longer than 12 hours:<br>If ([Indlagt]/EpiLPRFirstAdm) = 1 then course_in') %>%
    add_row(column_name = 'epilprfirst_out',       comment = 'First discharge date in LPR  related to the EpiLPRFirst_in (first admission date):<br>If ([Indlagt]/EpiLPRFirstAdm) = 1 then course_out') %>%
    add_row(column_name = 'epilprlastadm',         comment = 'Last admission within 14 days after your sample date and longer than 12 hours:<br>Last admission date in Epilpr (more than 12 hours long within 14 days after the sample date) = 1 ; If no admissions more than 12 hours long after within 14 days after the sample date = 0') %>%
    add_row(column_name = 'epilprlast_in',         comment = 'Last admission date in LPR within 14 days after the sample date and longer than 12 hours:<br>If ([Indlagt]/EpiLPRLastAdm) = 1 then course_in') %>%
    add_row(column_name = 'epilprlast_out',        comment = 'Last discharge date in LPR related to the EpiLPRLast_in (last admission date):<br>If ([Indlagt]/EpiLPRLastAdm) = 1 then course_out') %>%
    add_row(column_name = 'epilpricu_in',          comment = 'Start date of ICU contact (Identified by and ICU SOR code or by the contact related to an ICT procedure code). The admission has to occur within 14 days after the sample date:<br>Start date of ICU contact (Identified by and ICU SOR code or by an ICT procedure code)') %>%
    add_row(column_name = 'epilpricu_out',         comment = 'End date of ICU contact (Identified by and ICU SOR code or by an ICT procedure code) related to EpiLPRICU_inDT:<br>End date of ICU contact (Identified by and ICU SOR code or by an ICT procedure code)') %>%
    add_row(column_name = 'epilprict',             comment = 'First ICT treatment from an admission starting within 14 days after the sample date:<br>ICT treatment (procedure code :  NABB, NABE) = 1; else = 0') %>%
    add_row(column_name = 'epilprict_start',       comment = 'First ICT treatment date from an admission starting within 14 days after the sample date') %>%
    add_row(column_name = 'epilprresp',            comment = 'First Respiratory treatment from an admission starting within 14 days after the sample date:<br>Respiratory treatment (procedure code: Respiratorbehandling BGDA0) = 1; else = 0') %>%
    add_row(column_name = 'epilprresp_start',      comment = 'First Respiratory treatment date from an admission starting within 14 days after the sample date') %>%
    add_row(column_name = 'epilprvent',            comment = 'First Ventilation treatment from an admission starting within 14 days after the sample date:<br>Ventilation treatment (procedure code: Non-invasiv ventilation BGDA1) = 1; else = 0') %>%
    add_row(column_name = 'epilprvent_start',      comment = 'First Ventilation treatment date from an admission starting within 14 days after the sample date') %>%
    add_row(column_name = 'epilprecmo',            comment = 'First ECMO treatment from an admission starting within 14 days after the sample date:<br>ECMO treatment (procedure code: BGXA2)  = 1; else = 0') %>%
    add_row(column_name = 'epilprecmo_start',      comment = 'First ECMO treatment date from an admission starting within 14 days after the sample date') %>%
    add_row(column_name = 'epilprheart',           comment = 'First Heart treatment from an admission starting within 14 days after the sample date:<br>Heart treatment (procedure code: Behandling med inotropika/vasopressorer BFHC92, BFHC93, BFHC95) = 1; else = 0') %>%
    add_row(column_name = 'epilprheart_start',     comment = 'First Heart treatment date from an admission starting within 14 days after the sample date') %>%
    add_row(column_name = 'diabet',                comment = 'Diabetes from 5 years back in time from the sampling date:<br>Searched for ICD10-codes E10-E14  from 5 years back in time from the sampling date.') %>%
    add_row(column_name = 'adipos',                comment = 'Adipositas from 5 years back in time from the sampling date:<br>Searched for ICD10-codes E65-E68  from 5 years back in time from the sampling date.') %>%
    add_row(column_name = 'cancer',                comment = 'Cancer from 5 years back in time from the sampling date:<br>Searched for ICD10-codes Z85  from 5 years back in time from the sampling date. ') %>%
    add_row(column_name = 'neuro',                 comment = 'Neuro from 5 years back in time from the sampling date:<br>Searched for ICD10-codes G10-G14, G20-G23, G35-G37, G71-G73, G80-G83, G90-G91, G93-G96 G99, M51, men ikke G360 eller G902  from 5 years back in time from the sampling date.') %>%
    add_row(column_name = 'nyre',                  comment = 'Kidney  from 5 years back in time from the sampling date:<br>Searched for ICD10-codes N18-N200Y, Z992  from 5 years back in time from the sampling date.') %>%
    add_row(column_name = 'haem_c',                comment = 'Haem from 5 years back in time from the sampling date:<br>Searched for ICD10-codes Z856-Z857, C81-C96  from 5 years back in time from the sampling date.') %>%
    add_row(column_name = 'card_dis',              comment = 'Cardiac diseases from 5 years back in time from the sampling date:<br>Hjerte, ihd, valvul, hyper') %>%
    add_row(column_name = 'resp_dis',              comment = 'Respiratory diseases from 5 years back from the sampling date:<br>Kol, luftvej') %>%
    add_row(column_name = 'immu_dis',              comment = 'Immuno compromised diseases from 5 years back from the sampling date:<br>Hiv, immun, irradia, transpl') %>%
    add_row(column_name = 'other_risk',            comment = 'Other risk factors from 5 years back from the sampling date:<br>Alever, endo, hemo, koag, lever, medf, tb') %>%
    add_row(column_name = 'regadmission',          comment = 'Snapshot data from the regions:<br>1 = currently admitted') %>%
    #add_row(column_name = 'reg_regioncode',        comment = NA) %>%  # Auto
    add_row(column_name = 'reghospital',           comment = 'Hospital text from the snapshot of the regional data') %>%
    add_row(column_name = 'regdepartment',         comment = 'Department text from the snapshot of the regional data') %>%
    add_row(column_name = 'regicu',                comment = 'Currently in ICU from regional data (1=in)') %>%
    add_row(column_name = 'regfirst_in',           comment = 'First admission within 14 days after the sample date seen in the regional data') %>%
    add_row(column_name = 'regfirst_out',          comment = 'First discharge within 14 days after the sample date seen in the regional data') %>%
    add_row(column_name = 'firstadm',              comment = 'If epilpr first admission then EpiLPR data if among the positive case then completed by the regional data. Takes the earliest date from both') %>%
    add_row(column_name = 'firstdsc',              comment = 'If epilpr first discharge then EpiLPR data if among the positive case then completed by the regional data. Takes the oldest date from both') %>%
    add_row(column_name = 'lastadm',               comment = 'Regional snapshot data/LPR') %>%
    add_row(column_name = 'lastdsc',               comment = 'Regional snapshot data/LPR') %>%
    add_row(column_name = 'newlyadm',              comment = 'NewlyAdm = 1 - sample before admission or up to 48hours after tested for COVID -> Admission related to COVID; NewlyAdm = 2 - admitted and tested for COVID later or 48hrs after admission, COVID acquired in the hospital:<br>NewlyAdm = 1 and 2') %>%
    add_row(column_name = 'newlyadmdate',          comment = 'NewlyAdm = 2 then NewlyAdmDate = SampleDate; NewlyAdm = 1 then NewlyAdmDate = First date of admission:<br>Date of NewlyAdm') %>%
    add_row(column_name = 'regreport_time',        comment = 'Date and time when the region send data to SFTP-server') %>%
    add_row(column_name = 'travel',                comment = 'If any travel:<br>If [UdlandsrejseLand]/CountryOfTravel not "Denmark" or "No" = Y; else = N') %>%
    #add_row(column_name = 'first_vaccinedate',     comment = NA) %>% # Auto
    #add_row(column_name = 'first_vaccinename',     comment = NA) %>% # Auto
    add_row(column_name = 'pos_after_first_vacc',  comment = NA) %>%
    #add_row(column_name = 'second_vaccinedate',    comment = NA) %>% # Auto
    #add_row(column_name = 'second_vaccinename',    comment = NA) %>% # Auto
    add_row(column_name = 'pos_after_second_vacc', comment = NA) %>%
    add_row(column_name = 'branche_seneste',       comment = NA) %>%
    add_row(column_name = 'arb_kom_seneste',       comment = NA) %>%
    add_row(column_name = 'arb_reg_seneste',       comment = NA) %>%
    add_row(column_name = 'ydelse_seneste',        comment = NA) %>%
    add_row(column_name = 'batchid',               comment = NA) %>%
    add_row(column_name = 'timestamp',             comment = NA) %>%
    #add_row(column_name = 'checksum',              comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',               comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',              comment = NA) %>% # Auto
    add_row(column_name = 'firstadmclassv2',       comment = NA)

  check_docs(docs$covid_19_patientlinelist)
}


## prod.covid_19_wgs
{
  docs$covid_19_wgs <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 77 - covid-19 WGS data for modelgroup',
                            '',
                            'This report contains WGS data from SSI.',
                            'All samples that hae been sequenced.')) %>%
    #add_row(column_name = 'cpr',                  comment = NA) %>% # Auto
    #add_row(column_name = 'episodekey',           comment = NA) %>% # Auto
    add_row(column_name = 'date_linelist',        comment = NA) %>%
    #add_row(column_name = 'date_sampling',        comment = NA) %>% # Auto
    add_row(column_name = 'date_sequencing',      comment = NA) %>%
    #add_row(column_name = 'region',               comment = NA) %>% # Auto
    add_row(column_name = 'sequence_exclude',     comment = NA) %>%
    add_row(column_name = 'sequence_status',      comment = NA) %>%
    add_row(column_name = 'clade',                comment = NA) %>%
    #add_row(column_name = 'lineage',              comment = NA) %>% # Auto
    add_row(column_name = 'lineages_of_interest', comment = 'A grouping at the level of some chosen lineages - similar to focus_lineage from mg.miba') #%>%
    #add_row(column_name = 'who_variant',          comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',             comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',              comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',             comment = NA)     # Auto

  check_docs(docs$covid_19_wgs)
}


## prod.cpr3_t_adresse
{
  docs$cpr3_t_adresse <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '',
                            'Aktuelle adresseoplysninger')) #%>%
    #add_row(column_name = 'c_kom',              comment = NA) %>% # Auto
    #add_row(column_name = 'c_vej',              comment = NA) %>% # Auto
    #add_row(column_name = 'd_tilflyt_dato',     comment = NA) %>% # Auto
    #add_row(column_name = 'd_tilflyt_kom_dato', comment = NA) %>% # Auto
    #add_row(column_name = 'v_etage',            comment = NA) %>% # Auto
    #add_row(column_name = 'v_husnum',           comment = NA) %>% # Auto
    #add_row(column_name = 'v_pnr',              comment = NA) %>% # Auto
    #add_row(column_name = 'v_sidedoer',         comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',           comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',            comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',           comment = NA)     # Auto

  check_docs(docs$cpr3_t_adresse)}


## prod.cpr3_t_adresse_hist
{
  docs$cpr3_t_adresse_hist <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '',
                            'Historiske adresseoplysninger')) %>%
    add_row(column_name = 'c_annkor',           comment = 'Modified / deleted record.<br>K = modified, A = deleted, H = old municipality id') #%>%
    #add_row(column_name = 'c_kom',              comment = NA) %>% # Auto
    #add_row(column_name = 'c_vej',              comment = NA) %>% # Auto
    #add_row(column_name = 'd_fraflyt_dato',     comment = NA) %>% # Auto
    #add_row(column_name = 'd_fraflyt_kom_dato', comment = NA) %>% # Auto
    #add_row(column_name = 'd_tilflyt_dato',     comment = NA) %>% # Auto
    #add_row(column_name = 'd_tilflyt_kom_dato', comment = NA) %>% # Auto
    #add_row(column_name = 'v_etage',            comment = NA) %>% # Auto
    #add_row(column_name = 'v_husnum',           comment = NA) %>% # Auto
    #add_row(column_name = 'v_pnr',              comment = NA) %>% # Auto
    #add_row(column_name = 'v_sidedoer',         comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',           comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',            comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',           comment = NA)     # Auto

  check_docs(docs$cpr3_t_adresse_hist)
}


## prod.cpr3_t_beskyt
{
  docs$cpr3_t_beskyt <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '',
                            'Aktuel beskyttelse mod at blive kontaktet', 'Indeholder kun folk som er beskyttet - ikke alle personer i CPR')) %>%
    #add_row(column_name = 'v_pnr',    comment = NA) %>% # Auto
    add_row(column_name = 'c_type',   comment = 'Type of contact protection:<br>0001 = "Navne- og adressebeskyttelse", 0002 = "Lokalvejviserbeskyttelse", 0003 = "Reklamebeskyttelse", 0004 = "Forskerbeskyttelse"') %>%
    add_row(column_name = 'd_start',  comment = '@valid_from') %>%
    add_row(column_name = 'd_slet',   comment = '@valid_until') #%>%
    #add_row(column_name = 'checksum', comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',  comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts', comment = NA)     # Auto

  check_docs(docs$cpr3_t_beskyt)
}


## prod.cpr3_t_person
{
  docs$cpr3_t_person <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    #add_row(column_name = 'c_kon',              comment = NA) %>% # Auto
    #add_row(column_name = 'c_status',           comment = NA) %>% # Auto
    #add_row(column_name = 'd_foddato',          comment = NA) %>% # Auto
    add_row(column_name = 'd_status_hen_start', comment = '@valid_from') %>%
    add_row(column_name = 'v_far_pnr',          comment = '@cprnr - of the father') %>%
    add_row(column_name = 'v_mor_pnr',          comment = '@cprnr - of the mother') #%>%
    #add_row(column_name = 'v_pnr',              comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',           comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',            comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',           comment = NA) # Auto

  check_docs(docs$cpr3_t_person)
}


## prod.cpr3_t_person_status
{
  docs$cpr3_t_person_status <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '',
                            'Oversættelse af statuskode')) %>%
    #add_row(column_name = 'c_status',     comment = NA) %>% # Auto
    add_row(column_name = 'c_type',       comment = 'Either "AKTIV" (= person counts for population) or "INAKTIV" (= person does not count in population) ') %>%
    add_row(column_name = 'v_forklaring', comment = 'Description of the c_type variable') #%>%
    #add_row(column_name = 'checksum',     comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',      comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',     comment = NA) # Auto

  check_docs(docs$cpr3_t_person_status)
}


## prod.cpr3_t_sognkod
{
  docs$cpr3_t_sognkod <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '',
                            'Sognekoder og -tekster')) %>%
    #add_row(column_name = 'c_kom',       comment = NA) %>% # Auto
    #add_row(column_name = 'c_mynkod',    comment = NA) %>% # Auto
    #add_row(column_name = 'c_mynnvn',    comment = NA) %>% # Auto
    #add_row(column_name = 'c_vej',       comment = NA) %>% # Auto
    add_row(column_name = 'v_husnrfra',  comment = 'Start of the address range') %>%
    add_row(column_name = 'v_husnrtil',  comment = 'End of the address range') #%>%
    #add_row(column_name = 'v_ligeulige', comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',    comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',     comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',    comment = NA) # Auto

  check_docs(docs$cpr3_t_sognkod)
}


## prod.cpr_status
{
  docs$cpr_status <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains validation of the CPR numbers before they are anonymized',
                            '', '')) %>%
    #add_row(column_name = 'cpr',       comment = NA) %>% # Auto
    add_row(column_name = 'valid_cpr', comment = 'TRUE if the CPR number is a valid Danish CPR number, FALSE if not.')

  check_docs(docs$cpr_status)
}


## prod.logs
{
  docs$logs <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            '@table_description')) %>%
    add_row(column_name = 'date',            comment = NA) %>%
    #add_row(column_name = 'schema',          comment = NA) %>% # Auto
    #add_row(column_name = 'table',           comment = NA) %>% # Auto
    add_row(column_name = 'n_insertions',    comment = NA) %>%
    add_row(column_name = 'n_deactivations', comment = NA) %>%
    add_row(column_name = 'start_time',      comment = NA) %>%
    add_row(column_name = 'end_time',        comment = NA) %>%
    add_row(column_name = 'duration',        comment = NA) %>%
    add_row(column_name = 'success',         comment = NA) %>%
    add_row(column_name = 'log_file',        comment = NA)

  check_docs(docs$logs)
}


## prod.lpr3_sb_diagnose
{
  docs$lpr3_sb_diagnose <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    add_row(column_name = 'art',              comment = NA) %>%
    #add_row(column_name = 'diagnose_id',      comment = NA) %>% # Auto
    #add_row(column_name = 'fejl',             comment = NA) %>% # Auto
    add_row(column_name = 'fejl_kontakt',     comment = NA) %>%
    #add_row(column_name = 'kode',             comment = NA) %>% # Auto
    #add_row(column_name = 'kontakt_id',       comment = NA) %>% # Auto
    add_row(column_name = 'senereafkraeftet', comment = NA) %>%
    add_row(column_name = 'sideangivelse',    comment = NA) #%>%
    #add_row(column_name = 'checksum',         comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',          comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',         comment = NA) # Auto

  check_docs(docs$lpr3_sb_diagnose)
}


## prod.lpr3_sb_diagnose_tillaeg
{
  docs$lpr3_sb_diagnose_tillaeg <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    #add_row(column_name = 'diagnose_id',  comment = NA) %>% # Auto
    add_row(column_name = 'tillaegskode', comment = NA) #%>%
    #add_row(column_name = 'checksum',     comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',      comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',     comment = NA) # Auto

  check_docs(docs$lpr3_sb_diagnose_tillaeg)
}


## prod.lpr3_sb_kontakt
{
  docs$lpr3_sb_kontakt <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    add_row(column_name = 'aktionsdiagnose',      comment = NA) %>%
    add_row(column_name = 'ansvarlig_enhed',      comment = NA) %>%
    #add_row(column_name = 'ejerforhold',          comment = NA) %>% # Auto
    #add_row(column_name = 'fejl',                 comment = NA) %>% # Auto
    add_row(column_name = 'fejl_flb',             comment = NA) %>%
    add_row(column_name = 'fejl_trns',            comment = NA) %>%
    add_row(column_name = 'fritvalg',             comment = NA) %>%
    add_row(column_name = 'henvisendeinstans',    comment = NA) %>%
    add_row(column_name = 'henvisningsmaade',     comment = NA) %>%
    add_row(column_name = 'henvisningstidspunkt', comment = NA) %>%
    #add_row(column_name = 'hovedspeciale',        comment = NA) %>% # Auto
    add_row(column_name = 'inst_type',            comment = NA) %>%
    #add_row(column_name = 'institutionsejer',     comment = NA) %>% # Auto
    #add_row(column_name = 'kontakt_id',           comment = NA) %>% # Auto
    add_row(column_name = 'kontakttype',          comment = NA) %>%
    add_row(column_name = 'kontaktaarsag',        comment = NA) %>%
    #add_row(column_name = 'patient_foedselsdato', comment = NA) %>% # Auto
    #add_row(column_name = 'patient_koen',         comment = NA) %>% # Auto
    #add_row(column_name = 'personnummer',         comment = NA) %>% # Auto
    add_row(column_name = 'prioritet',            comment = NA) %>%
    #add_row(column_name = 'region',               comment = NA) %>% # Auto
    #add_row(column_name = 'sluttidspunkt',        comment = NA) %>% # Auto
    add_row(column_name = 'startbehandling',      comment = NA) #%>%
    #add_row(column_name = 'starttidspunkt',       comment = NA) %>% # Auto
    #add_row(column_name = 'sundhedsinstitution',  comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',             comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',              comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',             comment = NA) # Auto

  check_docs(docs$lpr3_sb_kontakt)
}


## prod.lpr3_sb_opholdsadresse
{
  docs$lpr3_sb_opholdsadresse <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    add_row(column_name = 'enhed',             comment = NA) %>%
    add_row(column_name = 'fejl_kontakt',      comment = NA) %>%
    add_row(column_name = 'fravaer',           comment = NA) %>%
    #add_row(column_name = 'kontakt_id',        comment = NA) %>% # Auto
    add_row(column_name = 'opholdsadresse_id', comment = NA) #%>%
    #add_row(column_name = 'sluttidspunkt',     comment = NA) %>% # Auto
    #add_row(column_name = 'starttidspunkt',    comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',          comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',           comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',          comment = NA) # Auto

  check_docs(docs$lpr3_sb_opholdsadresse)
}


## prod.lpr3_sb_procedurer
{
  docs$lpr3_sb_procedurer <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    #add_row(column_name = 'ejerforhold',         comment = NA) %>% # Auto
    #add_row(column_name = 'fejl',                comment = NA) %>% # Auto
    add_row(column_name = 'handlingsspec',       comment = NA) %>%
    add_row(column_name = 'indikation',          comment = NA) %>%
    #add_row(column_name = 'institutionsejer',    comment = NA) %>% # Auto
    #add_row(column_name = 'kode',                comment = NA) %>% # Auto
    #add_row(column_name = 'kontakt_id',          comment = NA) %>% # Auto
    add_row(column_name = 'kontrast',            comment = NA) %>%
    add_row(column_name = 'personale',           comment = NA) %>%
    add_row(column_name = 'procedurer_id',       comment = NA) %>%
    add_row(column_name = 'proceduretype',       comment = NA) %>%
    add_row(column_name = 'producerende_enhed',  comment = NA) %>%
    #add_row(column_name = 'region',              comment = NA) %>% # Auto
    add_row(column_name = 'sideangivelse',       comment = NA) #%>%
    #add_row(column_name = 'sluttidspunkt',       comment = NA) %>% # Auto
    #dd_row(column_name = 'starttidspunkt',      comment = NA) %>% # Auto
    #add_row(column_name = 'sundhedsinstitution', comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',            comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',             comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',            comment = NA) # Auto

  check_docs(docs$lpr3_sb_procedurer)
}


## prod.lpr3_sb_procedurer_tillaeg
{
  docs$lpr3_sb_procedurer_tillaeg <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    add_row(column_name = 'procedurer_id', comment = NA) %>%
    add_row(column_name = 'tillaegskode',  comment = NA) #%>%
    #add_row(column_name = 'checksum',      comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',       comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',      comment = NA) # Auto

  check_docs(docs$lpr3_sb_procedurer_tillaeg)
}


## prod.lpr3_sb_skskode
{
  docs$lpr3_sb_skskode <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    add_row(column_name = 'datoaendring',  comment = NA) %>%
    #add_row(column_name = 'gyldigfradato', comment = NA) %>% # Auto
    #add_row(column_name = 'gyldigtildato', comment = NA) %>% # Auto
    #add_row(column_name = 'kode',          comment = NA) %>% # Auto
    add_row(column_name = 'langtext',      comment = NA) %>%
    add_row(column_name = 'text',          comment = NA) #%>%
    #add_row(column_name = 'checksum',      comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',       comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',      comment = NA) # Auto

  check_docs(docs$lpr3_sb_skskode)
}


## prod.lpr3_sb_sor_complete
{
  docs$lpr3_sb_sor_complete <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the SDS data source:',
                            '', '')) %>%
    #add_row(column_name = 'gaeldende_fra_dato', comment = NA) %>% # Auto
    #add_row(column_name = 'hovedspeciale',      comment = NA) %>% # Auto
    add_row(column_name = 'ophoert_dato',       comment = NA) %>%
    add_row(column_name = 'parent_sorkode',     comment = NA) %>%
    #add_row(column_name = 'region',             comment = NA) %>% # Auto
    add_row(column_name = 'shak_kode',          comment = NA) %>%
    add_row(column_name = 'sidst_aendret_dato', comment = NA) %>%
    add_row(column_name = 'sor_type',           comment = NA) %>%
    add_row(column_name = 'sorkode',            comment = NA) %>%
    add_row(column_name = 'speciale2',          comment = NA) %>%
    add_row(column_name = 'speciale3',          comment = NA) %>%
    add_row(column_name = 'speciale4',          comment = NA) %>%
    add_row(column_name = 'speciale5',          comment = NA) %>%
    add_row(column_name = 'speciale6',          comment = NA) %>%
    add_row(column_name = 'speciale7',          comment = NA) %>%
    add_row(column_name = 'speciale8',          comment = NA) #%>%
    #dd_row(column_name = 'checksum',           comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',            comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',           comment = NA) # Auto

  check_docs(docs$lpr3_sb_sor_complete)
}


## prod.maalgruppe_doed
{
  docs$maalgruppe_doed <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 62 - covid-19 målgruppedata with status for modelgroup',
                            '',
                            'This standard report contains information about malgruppe to the modelgroup on SSI.',
                            'All CPR numbers and status (dead, alive) who are in the target group for the covid-19 vaccination.')) %>%
    #add_row(column_name = 'cpr',                 comment = NA) %>% # Auto
    #add_row(column_name = 'maalgruppe_id',       comment = NA) %>% # Auto
    add_row(column_name = 'ny_maalgruppe_kombi', comment = NA) %>%
    add_row(column_name = 'alive',               comment = NA) #%>%
    #add_row(column_name = 'checksum',            comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',             comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',            comment = NA) # Auto

  check_docs(docs$maalgruppe_doed)
}


## prod.municipalities
{
  docs$municipalities <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            '@table_description')) %>%
    #add_row(column_name = 'municipality_id', comment = NA) %>% # Auto
    #add_row(column_name = 'municipality',    comment = NA) %>% # Auto
    add_row(column_name = 'municipality_en', comment = '@municipality - English locale') %>%
    #add_row(column_name = 'province_id',     comment = NA) %>% # Auto
    #add_row(column_name = 'province',        comment = NA) %>% # Auto
    add_row(column_name = 'province_en',     comment = '@province - English locale') %>%
    #add_row(column_name = 'region_id',       comment = NA) %>% # Auto
    #add_row(column_name = 'region',          comment = NA) %>% # Auto
    add_row(column_name = 'region_en',       comment = '@region - English locale') #%>%
    #add_row(column_name = 'checksum',        comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',         comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',        comment = NA) # Auto

  check_docs(docs$municipalities)
}


## prod.patientlinelist_test
{
  docs$patientlinelist_test <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            '@table_description')) #%>%
    #add_row(column_name = 'cpr',              comment = NA) %>% # Auto
    #add_row(column_name = 'municipalitycode', comment = NA) %>% # Auto
    #add_row(column_name = 'episodekey',       comment = NA) %>% # Auto
    #add_row(column_name = 'checksum',         comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',          comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',         comment = NA) # Auto

  check_docs(docs$patientlinelist_test)
}


## prod.plejehjem_samples
{
  docs$plejehjem_samples <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 64 - covid-19 plejehjem for modelgroup',
                            '',
                            'This report contains nursing home residents who got tested for covid-19 tests and information about the nursing home.',
                            'CPR on residents of a nursing home registered in Plejehjemsoversigten.')) %>%
    #add_row(column_name = 'cpr',               comment = NA) %>% # Auto
    add_row(column_name = 'larlab',            comment = NA) %>%
    add_row(column_name = 'plejehjem_doed',    comment = NA) %>%
    add_row(column_name = 'plejehjemsid',      comment = NA) %>%
    #add_row(column_name = 'sampledate',        comment = NA) %>% # Auto
    add_row(column_name = 'plejehjem',         comment = NA) %>%
    add_row(column_name = 'plejehjem_date',    comment = NA) %>%
    add_row(column_name = 'plejehjem_end',     comment = NA) %>%
    add_row(column_name = 'plejehjem_current', comment = NA) %>%
    add_row(column_name = 'plejehjemsnavn',    comment = NA) #%>%
    #add_row(column_name = 'checksum',          comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',           comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',          comment = NA) # Auto

  check_docs(docs$plejehjem_samples)
}


## prod.snapshot_cpr
{
  docs$snapshot_cpr <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 67 - covid-19 snapshot on CPR-level for modelgroup',
                            '',
                            'This report contains information from the raw snapshot data from the regions on CPR-level. It includes all admission with CPR on a given day.',
                            'One row pr cpr')) %>%
    add_row(column_name = 'indberetningsdatotid',     comment = NA) %>%
    #add_row(column_name = 'cpr',                      comment = NA) %>% # Auto
    add_row(column_name = 'diagnosekoder',            comment = NA) %>%
    add_row(column_name = 'testet',                   comment = NA) %>%
    add_row(column_name = 'hospital_tekst',           comment = NA) %>%
    add_row(column_name = 'hospital_sorkode',         comment = NA) %>%
    add_row(column_name = 'afdeling_tekst',           comment = NA) %>%
    add_row(column_name = 'afdeling_sorkode',         comment = NA) %>%
    add_row(column_name = 'datotid_indlaeggelse',     comment = '@valid_from - admission') %>%
    add_row(column_name = 'intensiv',                 comment = NA) %>%
    add_row(column_name = 'datotid_start_intensiv',   comment = '@valid_from - "intensiv" column') %>%
    add_row(column_name = 'datotid_slut_intensiv',    comment = '@valid_until - "intensiv" column') %>%
    add_row(column_name = 'respirator',               comment = NA) %>%
    add_row(column_name = 'datotid_start_respirator', comment = '@valid_from - "respirator" column') %>%
    add_row(column_name = 'dialyse',                  comment = NA) %>%
    add_row(column_name = 'datotid_start_dialyse',    comment = '@valid_from - "dialyse column') %>%
    add_row(column_name = 'doed',                     comment = 'TRUE if dead, FALSE if not') %>%
    add_row(column_name = 'datotid_doed',             comment = '@valid_from - death') %>%
    #add_row(column_name = 'regionskode',              comment = NA) %>% # Auto
    add_row(column_name = 'fileid',                   comment = NA) #%>%
    #add_row(column_name = 'checksum',                 comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',                  comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',                 comment = NA) # Auto

  check_docs(docs$snapshot_cpr)
}


## prod.snapshot_hospital
{
  docs$snapshot_hospital <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 66 - covid-19 aggregated snapshot for modelgroup',
                            '',
                            'This report contains information from the raw snapshot data from the regions. It includes aggregred information about admissions and capacity.',
                            'One row pr hospital')) %>%
    add_row(column_name = 'expected_indberetningsdatotid', comment = NA) %>%
    add_row(column_name = 'indberetningsdatotid_hospital', comment = NA) %>%
    add_row(column_name = 'fileid_hospital',               comment = NA) %>%
    add_row(column_name = 'indberetningsdatotid_cpr',      comment = NA) %>%
    add_row(column_name = 'fileid_cpr',                    comment = NA) %>%
    add_row(column_name = 'nost_rundate',                  comment = NA) %>%
    add_row(column_name = 'nost_batchid',                  comment = NA) %>%
    #add_row(column_name = 'regionskode',                   comment = NA) %> %# Auto
    add_row(column_name = 'hospital_tekst',                comment = NA) %>%
    add_row(column_name = 'totalkapacitet',                comment = NA) %>%
    add_row(column_name = 'totaloptaget_ialt',             comment = NA) %>%
    add_row(column_name = 'total_covid',                   comment = NA) %>%
    add_row(column_name = 'intensivkapacitet',             comment = NA) %>%
    add_row(column_name = 'intensivoptaget_ialt',          comment = NA) %>%
    add_row(column_name = 'intensiv_covid',                comment = NA) %>%
    add_row(column_name = 'respiratorkapacitet',           comment = NA) %>%
    add_row(column_name = 'respiratoroptaget_ialt',        comment = NA) %>%
    add_row(column_name = 'respirator_covid',              comment = NA) %>%
    add_row(column_name = 'intensiv_respirator_covid',     comment = NA) %>%
    add_row(column_name = 'definition',                    comment = NA) %>%
    add_row(column_name = 'timestamp',                     comment = NA) #%>%
    #add_row(column_name = 'checksum',                      comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',                       comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',                      comment = NA) # Auto

  check_docs(docs$snapshot_hospital)
}


## prod.stps_data
{
  docs$stps_data <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 71 - covid-19 STPS data for modelgroup',
                            '',
                            'This report contains data from contracttracing at Styrelsen for patientsikkerhed (STPS).',
                            'All data from questionnaire data from STPS.')) %>%
    #add_row(column_name = 'cpr',                           comment = NA) %>% # Auto
    add_row(column_name = 'formodetsmittekilde',           comment = NA) %>%
    add_row(column_name = 'formodet_smittekilde_kategori', comment = NA) %>%
    add_row(column_name = 'kendtsmittekilde',              comment = NA) %>%
    add_row(column_name = 'harsymptomer',                  comment = NA) %>%
    add_row(column_name = 'symptomdebut',                  comment = NA) %>%
    add_row(column_name = 'formodetsmittetudlandet',       comment = NA) %>%
    add_row(column_name = 'udlandsophold',                 comment = NA) %>%
    add_row(column_name = 'udlandsopholdlandnavn',         comment = NA) #%>%
    #add_row(column_name = 'checksum',                      comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',                       comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',                      comment = NA) # Auto

  check_docs(docs$stps_data)
}


## prod.test
{
  docs$test <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            '@table_description')) %>%
    add_row(column_name = 'f1',       comment = NA) #%>%
    #add_row(column_name = 'from_ts',  comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts', comment = NA) %>% # Auto
    #add_row(column_name = 'checksum', comment = NA) # Auto

  check_docs(docs$test)
}


## prod.variant_covid_19
{
  docs$variant_covid_19 <- tibble(column_name = character(), comment = character()) %>%
    add_row(comment = paste(sep = '<br>',
                            'This table contains daily updates of the DIAS data source:',
                            'ReportDefinitionId 68 - covid-19 variants for modelgroup',
                            '',
                            'This report contains information about all variants for covid-19 in EpiMiBa from november 2021 and forward.',
                            'A CPR can have more rows.')) %>%
    #add_row(column_name = 'cprnr10',           comment = NA) %>% # Auto
    #add_row(column_name = 'prdate_adjusted',   comment = NA) %>% # Auto
    add_row(column_name = 'suspected_omicron', comment = NA) %>%
    add_row(column_name = 'confirmed_omicron', comment = NA) #%>%
    #add_row(column_name = 'checksum',          comment = NA) %>% # Auto
    #add_row(column_name = 'from_ts',           comment = NA) %>% # Auto
    #add_row(column_name = 'until_ts',          comment = NA) # Auto


  check_docs(docs$variant_covid_19)
}


# Look for tables that have moved away from prod (or have been deleted)
prod_tables <- get_tables(conn) %>%
  filter(schema == 'prod') %>%
  pull(table)

table_deleted <- names(docs)[names(docs) %notin% prod_tables]
map(table_deleted, ~ paste0('table ', .x, ' no longer in schema "prod"')) %>%
  reduce(union, .init = '') %>% printr(sep = '\n')

# Look for new tables that are not documented
table_added <- prod_tables[prod_tables %notin% names(docs)]
map(table_added, ~ paste0('table ', .x, ' in schema "prod" but not documented here')) %>%
  reduce(union, .init = '') %>% printr(sep = '\n')




# Set init to true, if you want the docs to be timestamped from when the tables were created
# This should only ever be done the first time the docs are created.
init <- FALSE

# Get log information for tables
{
  db_logs <- get_table(conn, "prod.logs") %>%
    filter(success, is.na(schema)) # Only successful runs for Piotr's tables
}

for (target_table in names(docs)) {

  # Check for template comments
  auto_docable <- docs[[target_table]] %>% left_join(get_table(conn, 'docs.templates') %>% collect(), suffix = c('.', '.template'), by = 'column_name') %>% filter(!is.na(comment.template))
  if (nrow(auto_docable) > 0) pmap(auto_docable, ~ warning('column', ..1, ' in table', target_table, ' can be auto-commented with:', ..3))

  # Determine timestamp for the docs
  if (init) {
    slice_ts <- db_logs %>% filter(is.na(schema), table == target_table) %>% slice_min(date, with_ties = F) %>% pull(date) %>% as.character()
  } else {
    slice_ts <- db_logs %>% filter(is.na(schema), table == target_table) %>% slice_max(date, with_ties = F) %>% pull(date) %>% as.character()
  }

  # Use fallback date
  if (length(slice_ts) == 0)  {
    if (init) {
      slice_ts <- db_logs %>% slice_min(date, with_ties = F) %>% pull(date) %>% as.character()
    } else {
      slice_ts <- paste(today(), '09:00:00')
    }
  }

  # Commit to DB
  log <- capture.output(
    pmap(filter(docs[[target_table]], !is.na(comment)), ~ db_comment(conn, paste('prod', target_table, sep = '.'), column = ..1, comment = ..2, timestamp = slice_ts)))
  failed <- log[!is.na(str_extract(log, '(WARNING|ERROR)'))]
  if (length(failed) > 0) print(failed)
  auto_comment(conn, paste('prod', target_table, sep = '.'), timestamp = slice_ts)
}


close_connection(conn)
