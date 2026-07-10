# This files takes a specific commit of Diseasy and installs it and the
# dependencies at this state

# Note: This was primarily vibe coded with ChatGPT


# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)


github_repo <- "ssi-dk/diseasy"
github_commit <- "3bf17232b9eb8c1114de46036d7c715ef52b2b88"

source_repos <- c("https://cloud.r-project.org")

repo_dir <- file.path(getwd(), "offline-repo")

read_stored_diseasy_sha <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  stored_sha <- trimws(readLines(path, warn = FALSE, n = 1L))

  if (length(stored_sha) == 0L || !nzchar(stored_sha)) {
    return(NA_character_)
  }

  stored_sha
}

diseasy_sha_file <- file.path(repo_dir, "diseasy-sha.txt")
stored_diseasy_sha <- read_stored_diseasy_sha(diseasy_sha_file)

offline_repo_is_current <- identical(stored_diseasy_sha, github_commit)

if (offline_repo_is_current) {
  message("Offline repo is already current for diseasy SHA: ", github_commit)
  message("Repo: ", normalizePath(repo_dir, winslash = "/", mustWork = TRUE))
  message("SHA file: ", normalizePath(diseasy_sha_file, winslash = "/", mustWork = TRUE))
} else {
  if (!curl::has_internet()) {
    stop("No internet available to update offline-repo")
  }
  if (is.na(stored_diseasy_sha)) {
    message("No stored diseasy SHA found in offline repo. Creating/updating repo.")
  } else {
    message(
      "Stored diseasy SHA ",
      stored_diseasy_sha,
      " differs from requested SHA ",
      github_commit,
      ". Creating/updating repo."
    )
  }

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Required package is not installed: jsonlite", call. = FALSE)
}

checkout_dir <- file.path(repo_dir, "_github-checkout")
source_dir <- file.path(repo_dir, "src", "contrib")

dir.create(checkout_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

archive_url <- sprintf(
  "https://github.com/%s/archive/%s.tar.gz",
  github_repo,
  github_commit
)

archive_file <- file.path(
  checkout_dir,
  paste0("github-", gsub("[^A-Za-z0-9_.-]", "-", github_commit), ".tar.gz")
)

github_repo_name <- basename(github_repo)
expected_checkout_dir <- file.path(
  checkout_dir,
  paste0(github_repo_name, "-", github_commit)
)

checkout_candidates <- list.dirs(
  path = checkout_dir,
  recursive = FALSE,
  full.names = TRUE
)

stale_checkout_dirs <- setdiff(checkout_candidates, expected_checkout_dir)

if (length(stale_checkout_dirs) > 0L) {
  file.remove(stale_checkout_dirs)
}

if (!file.exists(archive_file)) {

  message("Downloading GitHub archive...")

  utils::download.file(
    url = archive_url,
    destfile = archive_file,
    mode = "wb",
    quiet = FALSE
  )
}

if (!file.exists(file.path(expected_checkout_dir, "DESCRIPTION"))) {
  utils::untar(
    tarfile = archive_file,
    exdir = checkout_dir
  )
}

if (file.exists(file.path(expected_checkout_dir, "DESCRIPTION"))) {
  package_root <- expected_checkout_dir
} else {
  checkout_candidates <- list.dirs(
    path = checkout_dir,
    recursive = FALSE,
    full.names = TRUE
  )

  package_roots <- checkout_candidates[
    file.exists(file.path(checkout_candidates, "DESCRIPTION"))
  ]

  if (length(package_roots) != 1L) {
    stop(
      "Expected exactly one package root with DESCRIPTION, found: ",
      length(package_roots),
      call. = FALSE
    )
  }

  package_root <- package_roots[[1L]]
}

checkout_sha <- sub(
  pattern = paste0("^", github_repo_name, "-"),
  replacement = "",
  x = basename(package_root)
)

if (!identical(checkout_sha, github_commit)) {
  stop(
    "Checkout folder SHA does not match requested diseasy SHA. Folder has ",
    checkout_sha,
    "; requested ",
    github_commit,
    ".",
    call. = FALSE
  )
}

lockfile_path <- file.path(package_root, "pak.lock")

if (!file.exists(lockfile_path)) {
  stop(
    "pak.lock not found in package root: ",
    package_root,
    call. = FALSE
  )
}

invisible(file.copy(
  from = lockfile_path,
  to = dirname(repo_dir),
  overwrite = TRUE
))

message("Using lockfile: ", normalizePath(lockfile_path, winslash = "/"))

lockfile <- jsonlite::read_json(
  path = lockfile_path,
  simplifyVector = TRUE
)

package_table <- as.data.frame(lockfile[["packages"]])

required_lockfile_columns <- c("package", "version")

missing_lockfile_columns <- setdiff(
  required_lockfile_columns,
  names(package_table)
)

if (length(missing_lockfile_columns) > 0L) {
  stop(
    "Lockfile is missing required package columns: ",
    paste(missing_lockfile_columns, collapse = ", "),
    call. = FALSE
  )
}

local_description <- read.dcf(file.path(package_root, "DESCRIPTION"))
local_package <- unname(local_description[1L, "Package"])
local_version <- unname(local_description[1L, "Version"])

locked_local_version <- package_table[["version"]][
  package_table[["package"]] == local_package
]

if (length(locked_local_version) == 1L && locked_local_version != local_version) {
  stop(
    "Local package version mismatch. DESCRIPTION has ",
    local_package,
    " ",
    local_version,
    ", but lockfile has ",
    locked_local_version,
    ".",
    call. = FALSE
  )
}

base_packages <- rownames(installed.packages(priority = "base"))

remote_package_table <- package_table[
  !package_table[["package"]] %in% c(local_package, base_packages),
  ,
  drop = FALSE
]

message("Source repositories:")
message(paste0("  - ", source_repos, collapse = "\n"))

message("Downloading exact source packages...")

download_results <- vector("list", nrow(remote_package_table))
download_failures <- character()

for (row_index in seq_len(nrow(remote_package_table))) {
  package <- remote_package_table[["package"]][[row_index]]
  version <- remote_package_table[["version"]][[row_index]]
  filename <- paste0(package, "_", version, ".tar.gz")
  destfile <- file.path(source_dir, filename)

  if (file.exists(destfile)) {
    download_results[[row_index]] <- data.frame(
      "package" = package,
      "version" = version,
      "url" = NA_character_,
      "file" = normalizePath(destfile, winslash = "/", mustWork = TRUE)
    )

    next
  }

  temp_file <- tempfile(pattern = package, fileext = ".tar.gz")

  message("[", row_index, "/", nrow(remote_package_table), "] ", package, " ", version)

  package_urls <- unlist(lapply(
    source_repos,
    function(repo) {
      repo <- sub("/+$", "", repo)

      c(
        paste0(repo, "/src/contrib/", filename),
        paste0(repo, "/src/contrib/Archive/", package, "/", filename)
      )
    }
  ))

  package_downloaded <- FALSE
  attempt_messages <- character()

  for (package_url in package_urls) {
    if (file.exists(temp_file)) {
      unlink(temp_file)
    }

    download_attempt <- tryCatch(
      {
        utils::download.file(
          url = package_url,
          destfile = temp_file,
          mode = "wb",
          quiet = TRUE
        )
      },
      error = function(error) {
        error
      },
      warning = function(warning) {
        warning
      }
    )

    if (inherits(download_attempt, "condition")) {
      attempt_messages <- c(
        attempt_messages,
        paste0("  - ", package_url, " -> ", conditionMessage(download_attempt))
      )

      next
    }

    temp_dir <- tempfile(pattern = "tarball-validation-")
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

    description_path <- paste0(package, "/DESCRIPTION")

    validation_attempt <- tryCatch(
      {
        utils::untar(
          tarfile = temp_file,
          files = description_path,
          exdir = temp_dir
        )

        extracted_description <- file.path(temp_dir, description_path)

        if (!file.exists(extracted_description)) {
          stop(
            "No DESCRIPTION file found at expected path inside source tarball: ",
            description_path
          )
        }

        description <- read.dcf(extracted_description)

        actual_package <- unname(description[1L, "Package"])
        actual_version <- unname(description[1L, "Version"])

        if (!identical(actual_package, package)) {
          stop(
            "Package mismatch. Expected ",
            package,
            ", got ",
            actual_package,
            "."
          )
        }

        if (!identical(actual_version, version)) {
          stop(
            "Version mismatch. Expected ",
            version,
            ", got ",
            actual_version,
            "."
          )
        }

        TRUE
      },
      error = function(error) {
        error
      },
      warning = function(warning) {
        warning
      }
    )

    unlink(temp_dir, recursive = TRUE, force = TRUE)

    if (inherits(validation_attempt, "condition")) {
      attempt_messages <- c(
        attempt_messages,
        paste0("  - ", package_url, " -> ", conditionMessage(validation_attempt))
      )

      next
    }

    file.copy(
      from = temp_file,
      to = destfile,
      overwrite = TRUE
    )

    download_results[[row_index]] <- data.frame(
      "package" = package,
      "version" = version,
      "url" = package_url,
      "file" = normalizePath(destfile, winslash = "/", mustWork = TRUE)
    )

    package_downloaded <- TRUE

    break
  }

  if (!package_downloaded) {
    download_failures <- c(
      download_failures,
      paste0(
        package,
        " ",
        version,
        "\n",
        paste(attempt_messages, collapse = "\n")
      )
    )
  }
}

if (length(download_failures) > 0L) {
  stop(
    "Failed to download exact source tarballs for these packages:\n\n",
    paste(download_failures, collapse = "\n\n"),
    call. = FALSE
  )
}

download_results <- do.call(rbind, download_results)

message("Building local package: ", local_package, " ", local_version)

expected_local_tarball <- file.path(
  source_dir,
  paste0(local_package, "_", local_version, ".tar.gz")
)

unlink(expected_local_tarball, force = TRUE)

old_wd <- getwd()
setwd(source_dir)

build_output <- system2(
  command = file.path(R.home("bin"), "R"),
  args = c(
    "CMD",
    "build",
    "--no-build-vignettes",
    "--no-manual",
    shQuote(normalizePath(package_root, winslash = "/", mustWork = TRUE))
  ),
  stdout = TRUE,
  stderr = TRUE
)

setwd(old_wd)

build_status <- attr(build_output, "status")

if (!is.null(build_status) && build_status != 0L) {
  stop(
    "R CMD build failed:\n",
    paste(build_output, collapse = "\n"),
    call. = FALSE
  )
}

if (!file.exists(expected_local_tarball)) {
  stop(
    "R CMD build completed, but expected tarball was not found: ",
    expected_local_tarball,
    "\nBuild output:\n",
    paste(build_output, collapse = "\n"),
    call. = FALSE
  )
}

temp_dir <- tempfile(pattern = "local-tarball-validation-")
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

description_path <- paste0(local_package, "/DESCRIPTION")

utils::untar(
  tarfile = expected_local_tarball,
  files = description_path,
  exdir = temp_dir
)

extracted_description <- file.path(temp_dir, description_path)

if (!file.exists(extracted_description)) {
  stop(
    "No DESCRIPTION file found in built local package tarball.",
    call. = FALSE
  )
}

built_description <- read.dcf(extracted_description)

unlink(temp_dir, recursive = TRUE, force = TRUE)

built_package <- unname(built_description[1L, "Package"])
built_version <- unname(built_description[1L, "Version"])

if (!identical(built_package, local_package)) {
  stop("Built local package tarball has wrong Package field.", call. = FALSE)
}

if (!identical(built_version, local_version)) {
  stop("Built local package tarball has wrong Version field.", call. = FALSE)
}

download_results <- rbind(
  download_results,
  data.frame(
    "package" = local_package,
    "version" = local_version,
    "url" = paste0(github_repo, "@", github_commit),
    "file" = normalizePath(expected_local_tarball, winslash = "/", mustWork = TRUE)
  )
)

message("Writing repository index...")

tools::write_PACKAGES(
  dir = source_dir,
  type = "source",
  latestOnly = FALSE
)

packages_file <- file.path(source_dir, "PACKAGES")

if (!file.exists(packages_file)) {
  stop(
    "Repository index does not exist: ",
    packages_file,
    call. = FALSE
  )
}

packages_index <- read.dcf(packages_file)

repo_index <- data.frame(
  "package" = unname(packages_index[, "Package"]),
  "version" = unname(packages_index[, "Version"])
)

repo_versions <- stats::setNames(
  repo_index[["version"]],
  repo_index[["package"]]
)

locked_versions <- stats::setNames(
  verify_package_table[["version"]],
  verify_package_table[["package"]]
)


missing_packages <- setdiff(
  paste(names(locked_versions), locked_versions),
  paste(names(repo_versions), repo_versions)
)

if (length(missing_packages) > 0L) {
  stop(
    "Missing packages in offline repo: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

manifest_path <- file.path(repo_dir, "download-manifest.csv")

utils::write.csv(
  x = download_results,
  file = manifest_path,
  row.names = FALSE
)

# Copy the optimiser script from the current diseasy version
optimiser_source <- file.path(
  package_root,
  "data-raw",
  "diseasy_immunity_optimiser_results.R"
)
optimiser_dest <- file.path(getwd(), "scripts", "3_diseasy_immunity_optimisation.R")

optimiser_copied <- file.copy(
  from = optimiser_source,
  to = optimiser_dest,
  overwrite = TRUE
)

if (!optimiser_copied) {
  stop(
    "Failed to copy optimiser script from ",
    optimiser_source,
    " to ",
    optimiser_dest,
    ".",
    call. = FALSE
  )
}

writeLines(github_commit, diseasy_sha_file, useBytes = TRUE)

message("Offline repo created successfully.")
message("Repo: ", normalizePath(repo_dir, winslash = "/", mustWork = TRUE))
message("Source packages: ", normalizePath(source_dir, winslash = "/", mustWork = TRUE))
message("Manifest: ", normalizePath(manifest_path, winslash = "/", mustWork = TRUE))
message("Diseasy SHA: ", github_commit)
message("SHA file: ", normalizePath(diseasy_sha_file, winslash = "/", mustWork = TRUE))
}
