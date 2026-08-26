# This files installs the dependencies of our reference diseasy commit into a local library and updates library path
# Note: This was primarily vibe coded with ChatGPT

# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
setwd(wd)



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
package_table[["library"]] <- NULL

# Remove heavy DB packages that takes long time to install
package_table <- package_table[!(package_table[["package"]] %in% c("duckdb", "RSQLite")), ]

package_table$installed <- sapply(package_table[["package"]], \(p) tryCatch(as.character(packageVersion(p)), error = \(e) ""))
package_table <- package_table[
  package_table[["version"]] != package_table[["installed"]],
]

if (!rlang::is_installed("purrr")) {
  install.packages(
    pkgs = "purrr",
    type = "source",
    dependencies = FALSE,
    quiet = TRUE
  )
}

is_installed <- function(package, version) {
  if (!rlang::is_installed(package)) {
    return(FALSE)
  }

  installed_version <- as.character(packageVersion(package))
  requested_version <- stringr::str_replace(version, "-", ".")

  return(rlang::is_installed(package) && installed_version == requested_version)
}


for (attempt in range(5)) {
  packages_to_install <- package_table
  packages_to_install <- packages_to_install[
    !purrr::map2_lgl(packages_to_install[["package"]], packages_to_install[["version"]], is_installed),
  ]
  packages_to_install[["r"]] <- seq_len(nrow(packages_to_install))

  if (nrow(packages_to_install) > 0) {
    message("Installing packages from offline repo (compiling from source -- will take time!)")
    cache <- getOption("diseasy.cache")
    cache$reset()
  } else {
    message("Package library up to date!")
    break
  }

  purrr::pwalk(
    packages_to_install,
    \(package, version, binary, installed, r) {
      message(glue::glue("Installing package: [{r}/{nrow(packages_to_install)}] {package} v{version} ..."))
      install.packages(
        pkgs = package,
        type = "source",
        dependencies = FALSE,
        quiet = TRUE
      )
    }
  )
}


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
  ~ as.character(packageVersion(.))
) |>
  stats::setNames(names(locked_versions))

missing_packages <- setdiff(
  paste(names(installed_versions), stringr::str_replace(installed_versions, "-", ".")),
  paste(names(locked_versions), stringr::str_replace(locked_versions, "-", "."))
)

if (length(missing_packages) > 0L) {
  warning(
    "Missing packages in library!\n",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# Install extra packages needed to run the analysis scripts (but not used in diseasy)
withr::with_options(
  list("repos" = "https://cloud.r-project.org"),
  c("furrr", "rlang") |>
    purrr::discard(rlang::is_installed) |>
    install.packages(pkgs = _, quiet = TRUE)
)
