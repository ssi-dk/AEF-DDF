
relative_wd <- "manuscripts/DiseasyImmunity"

wd <- purrr::reduce(
  c(
    stringr::str_split(getwd(), .Platform$file.sep),
    stringr::str_split(relative_wd, "/")
  ),
  union
) |>
  paste(collapse = .Platform$file.sep)

withr::with_dir(
  new = wd,
  code = {
    source(file.path("scripts", "1_create_offline_repo.R"))
    source(file.path("scripts", "2_libPaths.R"))
    # source(file.path("scripts", "3_diseasy_immunity_optimisation")) # We leave this commented until NGC runs are downloaded locally
    #source(file.path("scripts", "4_0-analysis.R"))
  }
)
