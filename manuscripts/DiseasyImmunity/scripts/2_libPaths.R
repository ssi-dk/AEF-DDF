# This files installs the dependencies of our reference diseasy commit into a local library and updates library path
# Note: This was primarily vibe coded with ChatGPT

# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)



repo_dir <- file.path(getwd(), "offline-repo")

library_dir <- file.path(getwd(), "r-lib")
dir.create(library_dir, showWarnings = FALSE)

.libPaths(library_dir)

repo_url <- if (.Platform$OS.type == "windows") {
  paste0("file:///", repo_dir)
} else {
  paste0("file://", repo_dir)
}

options(
  repos = c(local = repo_url)
)

lockfile <- jsonlite::read_json(
  path = file.path(dirname(repo_dir), "pak.lock"),
  simplifyVector = TRUE
)

package_table <- as.data.frame(lockfile[["packages"]])

# Remove heavy DB packages that takes long time to install
package_table <- package_table[!(package_table[["package"]] %in% c("duckdb", "RSQLite")), ]

package_table <- package_table[
  !purrr::map2_lgl(
    package_table[["package"]],
    package_table[["version"]],
    ~ rlang::is_installed(.x) && packageVersion(.x) == .y
  )
  ,
]

install.packages(
  pkgs = package_table[["package"]],
  type = "source",
  dependencies = FALSE
)


base_packages <- rownames(installed.packages(priority = "base"))

verify_package_table <- package_table[
  !package_table[["package"]] %in% base_packages,
  ,
  drop = FALSE
]

# Verify installation state
locked_versions <- stats::setNames(
  verify_package_table[["version"]],
  verify_package_table[["package"]]
)

# Installed versions
installed_versions <- purrr::map_chr(
  names(locked_versions),
  packageVersion
) |>
  stats::setNames(names(locked_versions))

missing_packages <- setdiff(
  paste(names(installed_versions), installed_versions),
  paste(names(locked_versions), locked_versions)
)

if (length(missing_packages) > 0L) {
  stop(
    "Missing packages in library",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}
