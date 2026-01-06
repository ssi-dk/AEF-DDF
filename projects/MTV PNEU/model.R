# We first create a simple model without age_groups / serotype groupings




# Define model stratifications
age_cuts_lower <- 0 # Splits in the age groups

serotype_groups <- list( # Using german-splits for testing
  "G1" = c("4", "6B", "9V", "14", "18C", "19F", "23F"),
  "G2" = c("1", "5", "6A", "6C", "7F", "19A"),
  "G3" = "3",
  "G4" = c("22F", "33F"),
  "G5" = c("8", "10A", "11A", "12F", "15B"),
  "G6" = c("2", "9N", "17F", "20"),
  "G7" = "*"
)

carriage_states <- c(
  # Susceptible
  "S",

  # Single carriage
  names(serotype_groups),

  # Double carriage
  tidyr::expand_grid(names(serotype_groups), names(serotype_groups)) |>
    purrr::pmap_chr(~ paste(sort(union(..1, ..2)), collapse = "", sep = "")) |>
    unique() |>
    sort()
) |>
  unique()


