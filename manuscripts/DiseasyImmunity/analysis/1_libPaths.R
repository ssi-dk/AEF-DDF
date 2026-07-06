relative_repo_dir <- "manuscripts/DiseasyImmunity/offline-repo"

repo_dir <- purrr::reduce(
  c(
    stringr::str_split(getwd(), .Platform$file.sep),
    stringr::str_split(relative_repo_dir, "/")
  ),
  union
) |>
  paste(collapse = .Platform$file.sep)


library_dir <- file.path(dirname(repo_dir), "r-lib")
dir.create(library_dir, showWarnings = FALSE)

.libPaths(library_dir)

options(
  repos = c(local = repo_url)
)

lockfile <- jsonlite::read_json(
  path = file.path(dirname(repo_dir), "pak.lock"),
  simplifyVector = TRUE
)

package_table <- as.data.frame(lockfile[["packages"]])

install.packages(
  pkgs = package_table[["package"]],
  type = "source",
  dependencies = FALSE
)
