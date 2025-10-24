library(pbapply)
library(lubridate)
library(data.table)
library(stringr)
library(purrr)
#install.packages("rjson")
library(rjson)


# Path to the git directory:
# https://github.com/cov-lineages/pango-designation
setwd("C:/Users/B246705/Documents/Kode/R/cov_lineage/pango-designation")


# Get tagged commits from git directory
system2("git", "pull", stdout = TRUE)
commits_with_tags    <- system2("git", "show-ref --tags", stdout=TRUE)
commits_with_tags    <- unlist(lapply(commits_with_tags, \(.) str_extract(., "\\w+")))

commits_after_cutoff <- system2("git", "rev-list --tags --since='2022-05-13'", stdout=TRUE)

commits <- intersect(commits_with_tags, commits_after_cutoff)

# Determine the dates for the commits
commit_dates <- unlist(pblapply(commits, \(.)system2("git", paste("show -s --format=%ci", .), stdout=TRUE))) %>%
  parse_date_time("%Y-%m-%d %H:%M:%S %z", tz = "Europe/Copenhagen")



# Manually add specific commits
# If new variants have shown up in WGS data, but not yet in a tagged version, this
# will allow you to checkout the pango-designation git before the new tagged version is made

# Up until 2023-05-02, this was done by checking out to a time stamp,
# however, it was noticed that this did not work as intended and commits were not
# checking out correctly. These have been post-hoc changed to commit sha's that give the same
# output as the (previous) broken code. This means the date matching might not be perfect.

# The offending commits thus have a date they are "dated as" and an actual date the commit was from
# We keep their original dating in the naming of the files but note here when the data is actually from

extra_commits <- tibble::tibble(commit = character(0), date = as.Date(NA)) %>%
  dplyr::add_row(commit = "f1ccad99cbe94ec7592850bd4e6f4190824391ae", date = as.Date("2022-07-20")) %>%  # actually from 2022-07-08
  dplyr::add_row(commit = "e3dcb00a11539d1a78a7d70db9a11382982ff259", date = as.Date("2022-09-01")) %>%  # actually from 2022-08-19
  dplyr::add_row(commit = "15c4407736e8c3e9924893ad98c76fbe77c96b5c", date = as.Date("2022-09-07")) %>%  # actually from 2022-10-19
  dplyr::add_row(commit = "83742484b411b299f19d73d6fd294e0961baa274", date = as.Date("2022-11-18")) %>%  # actually from 2022-11-02
  dplyr::add_row(commit = "dd51c17cb5f5dc4551b5c4075604ff76da84d92f", date = as.Date("2022-12-21")) %>%  # actually from 2022-12-04
  dplyr::add_row(commit = "cc0d14be63a5a5ef743bf7686bdd5e56cc645cb4", date = as.Date("2023-02-09")) %>%  # actually from 2022-01-09
  dplyr::add_row(commit = "4008ec0053b29d2f30ea93fe56a641f816f4d05e", date = as.Date("2023-03-02")) %>%  # actually from 2023-02-14
  dplyr::add_row(commit = "d052e833ee6f04500ffdba43dfa6c84008d2258d", date = as.Date("2023-03-22")) %>%  # actually from 2023-02-28
  dplyr::add_row(commit = "a1a5f9f8b39cc6c080654b7fc56d0c4c44dcc647", date = as.Date("2023-04-24")) %>%
  dplyr::add_row(commit = "3bdba243b7e7108eb821bf8e0bd7fe0e46050fa1", date = as.Date("2023-06-20")) %>%
  dplyr::add_row(commit = "2a77486c04a4ed79cc37d7c9cb3cbf775b3d018b", date = as.Date("2023-09-30"))

commits <- c(commits, extra_commits$commit)

# Okay, so it turns out that as.Date() is not a very good function.
# Instead of just dropping the time part from a timestamp, it converts to some other timezone and takes the date there
# Why anyone would ever want that is a mystery.
# For us, this means that the extra_commit dates before "2023-04-24" are sometimes converted to the date before.
# and this means we now need some hacky code to produce consistent output files...
hacky_garbage <- map(extra_commits$date,
       ~ if (. < as.Date("2023-04-24")) {
         tmp <- as.POSIXct(., tz = "CET")
         hour(tmp) <- 0
         as.Date(tmp)
       } else {
         as.Date(.) # Yes, R will never respect the type of a Date object. It is simply incapable...
         # So we recast the Date back to a Date..
       })
commit_dates <- c(as.Date(commit_dates), hacky_garbage)

for (commit_id in seq_along(commits)) {
  system2("git", paste("checkout", commits[commit_id]), stdout=TRUE, stderr=TRUE)

  ## Get table with a line for each known variant
  lin_all <- read.table(file = "lineage_notes.txt", sep="\t",quote = "", fill=TRUE, header=TRUE)
  names(lin_all) <- tolower(names(lin_all))
  setDT(lin_all)

  # Get aliases in naming
  aliases <- fromJSON(file = "pango_designation/alias_key.json")

  # Recombinants are mapped to their name
  new_aliases <- map2(aliases, names(aliases), ~ ifelse(length(.x) == 1, .x, .y))
  names(new_aliases) <- names(aliases)

  alias_key <- data.table(prefix = names(new_aliases), expand = unlist(new_aliases) )
  alias_key[expand =="", expand := prefix] # To map "A" to "A"

  # Exclude lineages that are no longer in use
  lin_use <- lin_all[!grepl("*", lineage, fixed=TRUE)]

  # Expanding characters in front of first period using alias
  lin_use[, first_period := sapply(gregexpr(".", lineage, fixed = TRUE),min)] #  returning -1 if no period "."
  lin_use[, full := lineage]
  lin_use[first_period>0, prefix := substr(lineage, 1, first_period - 1)]
  lin_use[alias_key, on = "prefix", expand := i.expand]
  lin_use[first_period>0, full := paste0(expand,".", substr(lineage, first_period + 1, 1000) )]
  lin_use[, c("first_period", "expand", "prefix") := NULL]

  # Now all BA.5 subvariants can be found by:
  #lin_use[grepl(lin_use[lineage == "BA.5", full], full)]

  ## Adding WHO names
  # https://www.cdc.gov/coronavirus/2019-ncov/variants/variant-classifications.html  # 2022-07-25
  map_who <- data.table(who_variant = c("alpha",   "beta",    "gamma",      "delta",     "epsilon", "epsilon", "eta",     "iota",
                                        "kappa",     "zeta",      "mu",       "omicron"),
                        who_lineage = c("B.1.1.7", "B.1.351", "B.1.1.28.1", "B.1.617.2", "B.1.427", "B.1.429", "B.1.525", "B.1.526",
                                        "B.1.617.1", "B.1.1.28.2", "B.1.621", "B.1.1.529"))

  # Inspired from: https://stackoverflow.com/questions/51942694/data-table-merge-on-partial-match-of-different-columns-in-r
  tt <- map_who[, c( .SD, lin_use[grepl(who_lineage, full)]), by = 1:map_who[,.N]]
  lin_use[tt, on = "full", variant := who_variant]

  # Mapping is done
  save(lin_use, file = paste0("../lin_use_", commit_dates[commit_id], ".RData"))
}

system2("git", "checkout master", stdout=FALSE, stderr=FALSE)
