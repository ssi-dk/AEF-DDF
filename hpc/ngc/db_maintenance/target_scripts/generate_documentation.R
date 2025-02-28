#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(mg)
  library(mgdocumentation)
  library(devtools)
})

conn <- mg::get_connection()

# Get all of our tables in the database
tables <- mg::get_tables(conn) %>%
  tidyr::unite('db_table', schema, table, sep = '.')

# File paths...
pkg <- '/ngc/projects/ssi_mg/rassky/R_packages/R_documentation/'
file <- paste0(pkg, 'R/documentation.R')

# Add a header to the file
write('# This file is auto generated. Do not edit by hand..\n', file = file, append = F)

# Add the documentation
purrr::walk2(tables$db_table, seq_along(tables$db_table),
             ~ generate_documentation(conn, file, .x))

# Update the version number (use today's date)
description  <- readLines(paste0(pkg, 'DESCRIPTION'))
description  <- gsub(pattern = "(?<=^Version: )(.+$)", replace = today(), x = description, perl = T)
writeLines(description, con=paste0(pkg, 'DESCRIPTION'))

# Build documentation
suppressMessages({
  devtools::document(pkg = pkg, quiet = T)
  devtools::install( pkg = pkg, quiet = T)
})
