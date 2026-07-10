# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)


source(file.path("scripts", "1_create_offline_repo.R"))
source(file.path("scripts", "2_libPaths.R"))
# source(file.path("scripts", "3_diseasy_immunity_optimisation")) # We leave this commented until NGC runs are downloaded locally
#source(file.path("scripts", "4_0-analysis.R"))
