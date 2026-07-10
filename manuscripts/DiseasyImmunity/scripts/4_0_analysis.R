# TODOs
# Should we set individual_level = FALSE and / or monotonous = FALSE?

# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)



# Controls for the outputs

# Set figure targets
withr::local_options(
  "analysis.M_single" = c(2, 6, 10), # Facets
  "analysis.M_double" = 10 # Upper limit
)

# Set optimiser parameters
withr::local_options(
  "analysis.monotonous" = FALSE,
  "analysis.individual_level" = FALSE
)

# Controls for the computation
if (Sys.getenv("RSTUDIO") != "1") {
  workers <- unname(future::availableCores(omit = 1))
  future::plan("multicore", gc = TRUE, workers = workers)
}

# Setup a cache for the analysis
withr::local_options("diseasy.cache" = cachem::cache_disk(dir = "diseasy-cache/"))

# Source analysis scripts
source(file.path("scripts", "4_1-analysis-single-target.R"))
source(file.path("scripts", "4_2-analysis-double-targets.R"))
source(file.path("scripts", "4_3-analysis-simulation-study.R"))
