# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "Waning Immunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
setwd(wd)

# Setup a cache for the analysis
withr::local_options("diseasy.cache" = cachem::cache_disk(dir = "diseasy-cache/", max_size = Inf))

is_apptainer <- nzchar(Sys.getenv("APPTAINER_CONTAINER"))

if (!is_apptainer) {
  #source(file.path("scripts", "1_create_offline_repo.R"))
  #source(file.path("scripts", "2_libPaths.R"))
  #pak::pkg_install("ssi-dk/diseasy@af50b2e")
}

source(file.path("scripts", "3_0_analysis.R"))
