# This script generates zeroth and first-order reference scenarios for the world* using data from the
# Oxford COVID-19 Government Restriction Tracker project.
# "Reference scenario" refers to the actual restriction that were in society.
# The zeroth order scenario uses the (weighted average) stringency index to scale the overall risk over time and uses
# the "baseline" activity unit. That is, it uses the unmodified contact matrices for the country.

#* The intersection of countries tracked in the Oxford COVID-19 Government Restriction Tracker and in our
#  contact_basis data set.


# Get the Oxford COVID-19 Government Restriction Tracker index for stringency
oxford_data <- readr::read_csv(
  "https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_nat_latest.csv",
  col_types = list("Date" = "character"),
  show_col_types = FALSE
)


# Extract the relevant data from the Oxford data set
# We look only at the national level at the stringency and C* indices
oxford_subset <- oxford_data |>
  dplyr::filter(.data$Jurisdiction == "NAT_TOTAL") |>
  dplyr::mutate(
    "date" = as.Date(Date, "%Y%m%d"),
    "country_name" = .data$CountryName,
    "country_code" = countrycode::countrycode(.data$CountryName, origin = "country.name", destination = "iso2c"),
    "stringency" = .data$StringencyIndex_Average) |>
  dplyr::select("date", tidyselect::starts_with("country_"), "stringency", tidyselect::matches(r"{C\d.*}"))




# Now we subdivide into stringency data (for order 0) and C* data (for order 1)
stringency_data <- oxford_subset |>
  dplyr::select("date", tidyselect::starts_with("country_"), "stringency") # Stringency is "weighted average"






# For indices, we get the long form and group the flags by "school", "work", "home", and "other"
# To compute the sub indices according to the Oxford formula, we need the maximal values of each
# indicator and to know whether the indicator has a flag.
indicator_info <- tibble::tibble(
  indicator = c("C1",      "C2",      "C3",      "C4",      "C5",      "C6",      "C7",      "C8"),
  max_value = c( 3,         3,         2,         4,         2,         3,         2,         4),
  has_flag  = c( 1,         1,         1,         1,         1,         1,         1,         0),
  arena     = c("school",  "work",    "other",   "other",   "other",   "home", "other",   "other")
)

# Converting to long form
sub_index_data <- oxford_subset |>
  dplyr::select(!"stringency") |>
  tidyr::pivot_longer(!c("date", tidyselect::starts_with("country_")),
                      names_to = c("indicator", "target_group", "indicator_text"),
                      names_pattern = r"{(C\d)(\w*)_(.*)}")

# Interlacing data so flag is associated with each corresponding indicator
# (This could probably be done with a single pivot_longer above, but I could not
# get it to work, so this will have to do.)
sub_index_data <- dplyr::full_join(
  sub_index_data |>
    dplyr::filter(.data$indicator_text != "Flag"),
  sub_index_data |>
    dplyr::filter(.data$indicator_text == "Flag") |>
    dplyr::select(!"indicator_text") |>
    dplyr::rename("flag" = "value"),
  by = c("date", "country_name", "country_code", "indicator",
         "target_group")) |>
  dplyr::left_join(indicator_info, by = "indicator")


# According to Oxford, if a flag is NA it should not count towards the sub index value.
# This is done by setting the value to the has_flag value if the flag is NA.
# Similarly, if value is NA, it should count as zero
sub_index_data <- sub_index_data |>
  dplyr::mutate("flag" = dplyr::if_else(is.na(.data$flag), .data$has_flag, .data$flag),
                "value" = dplyr::if_else(is.na(.data$value), 0, .data$value))


# Now compute the sub index
sub_index_data <- sub_index_data |>
  dplyr::mutate("sub_index" = 100 * (value - 0.5 * (has_flag - flag)) / max_value)


# Now take simple averages within each arena
sub_index_data <- sub_index_data |>
  dplyr::summarize(sub_index = mean(sub_index),
                   .by = c("date", tidyselect::starts_with("country_"), "arena"))










# Some countries have stringency changes before "2020-01-01"
# Must note that in the description of the data set
stringency_data |>
  dplyr::group_by(country_code) |>
  dplyr::slice_min(date) |>
  dplyr::filter(stringency > 0)

# Similarly, some countries have sub_indices above 0 from the start
sub_index_data |>
  dplyr::group_by(country_code) |>
  dplyr::slice_min(date) |>
  dplyr::filter(sub_index > 0)



# The renaming mappings are the same for the two data sets, so we combine them for now
metric <- list(stringency_data, sub_index_data)
data_column <- c("stringency", "sub_index")


# Store only changes
metric_changes <- purrr::map2(metric, data_column, ~ {
  . |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(c("arena")))) |>
    dplyr::filter(.data[[.y]] != dplyr::lag(.data[[.y]]) | (dplyr::row_number() == 1 & .data[[.y]] > 0))
})


# Calibrate using Danish data
# For this, we use our detailed scenario for DK
dk_act <- diseasy::DiseasyActivity$new(activity_units = diseasy::dk_activity_units,
                                       contact_basis = diseasy::contact_basis$DK)
dk_act$change_activity(diseasy::dk_reference_scenario)
work_risk <- aggregate(social_distance_work ~ date, data = diseasy::dk_reference_scenario, FUN = mean)
dk_act$change_risk(date = work_risk$date, type = "work", risk = work_risk$social_distance_work)


# And get the degree of freedom over time in this scenario (per arena)
dk_scenario_freedom <- dk_act$get_scenario_openness(age_cuts_lower = c(0))

dk_freedom <- purrr::map2(dk_scenario_freedom, names(dk_scenario_freedom), ~ {
  tibble::enframe(.x, name = "arena", value = "freedom") |>
    dplyr::transmute("date" = as.Date(.y), .data$arena, "freedom" = unlist(.data$freedom))
  }) |>
  purrr::reduce(dplyr::union_all)



# We then extract the Danish metrics from our Oxford metrics
dk_metric <- purrr::map2(metric_changes, data_column, ~ {
  dplyr::filter(.x, .data$country_code == "DK") |>
    dplyr::select("date", tidyselect::any_of(c("arena")), !!.y)
})


# We compare and determine the "calibration factors", i.e., the scalar constants to multiply the
# metrics by to achieve the Danish metrics (on average)
calibration_factors <- purrr::map2(dk_metric, data_column, ~ {

  if ("arena" %in% colnames(.x)) {
    freedom <- dk_freedom
    by = c("arena", "date")
  } else {
    freedom <- dplyr::summarise(dk_freedom, "freedom" = mean(.data$freedom), .by = "date")
    by = c("date")
  }

  tibble::tibble(date = unique(c(freedom$date, .x$date))) |>
    dplyr::left_join(freedom, by = "date") |>
    dplyr::left_join(.x, by = by) |>
    dplyr::arrange(date) |>
    dplyr::filter(dplyr::if_any(.cols = tidyselect::any_of("arena"), .fns = purrr::negate(is.na))) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of("arena"))) |>
    dplyr::mutate(
      !!.y := dplyr::if_else(dplyr::row_number() == 1 & is.na(.data[[!!.y]]), 0, .data[[!!.y]]),
      "freedom" = zoo::na.locf(.data$freedom),
      !!.y := zoo::na.locf(.data[[!!.y]]),
      "freedom_proxy" = 1 - .data[[!!.y]] / 100,
      "k" = .data$freedom / .data$freedom_proxy) |>
    dplyr::filter(is.finite(.data$k)) |>
    dplyr::mutate(
      "weight" = as.numeric(difftime(dplyr::lead(date), date)),
      "weight" = dplyr::if_else(is.na(.data$weight),
                                as.numeric(difftime(as.Date(max(oxford_data$Date), "%Y%m%d"), .data$date)),
                                .data$weight)) |>
    dplyr::summarise(mean_k = sum(k * weight) / sum(weight)) |>
    dplyr::select(tidyselect::any_of(c("arena", "mean_k")))
})


world_freedoms <- purrr::map2(metric_changes, calibration_factors, ~ {

  if ("arena" %in% colnames(.x)) {
    join_fun <- purrr::partial(dplyr::left_join, by = "arena")
  } else {
    join_fun <- dplyr::cross_join
  }

  join_fun(.x, .y)
})


world_freedoms <- purrr::map2(world_freedoms, data_column, ~ {
  dplyr::mutate(.x, freedom = .data$mean_k * (1 - .data[[!!.y]] / 100)) |>
    dplyr::select("date", tidyselect::starts_with("country_"),
                  tidyselect::any_of("arena"), freedom)
})



world_reference_scenarios <- purrr::map(world_freedoms, \(freedom_order) {
  common_country_codes <- intersect(unique(freedom_order$country_code), names(diseasy::contact_basis))

  world_reference_scenario <- purrr::map(common_country_codes, \(country_code) {
    out <- freedom_order |>
      dplyr::filter(.data$country_code == !!country_code) |>
      dplyr::transmute(.data$date, "risk" = .data$freedom)

    if ("arena" %in% colnames(out)) {
      out <- out |>
        tidyr::pivot_wider(names_from = "arena", values_from = "risk", names_prefix = "risk_") |>
        dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("risk_"),
                                    .fns = ~ dplyr::if_else(dplyr::row_number() == 1 & is.na(.), max(., na.rm = TRUE), .))) |>
        dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("risk_"), .fns = zoo::na.locf))
    }

    out |>
      dplyr::mutate("opening" = dplyr::if_else(dplyr::row_number() == 1, "baseline", NA_character_),
                    "closing" = NA_character_,
                    .after = "date")
  })

  names(world_reference_scenario) <- common_country_codes
  return(world_reference_scenario)
})


world_reference_scenario_0 <- purrr::pluck(world_reference_scenarios, 1)
world_reference_scenario_1 <- purrr::pluck(world_reference_scenarios, 2)

saveRDS(world_reference_scenario_0,
        file.path("diseasy", "data sets", "world_reference_scenario", "world_reference_scenario_0.rds"))
saveRDS(world_reference_scenario_1,
        file.path("diseasy", "data sets", "world_reference_scenario", "world_reference_scenario_1.rds"))




# Configure a Swedish activity scenario
se_reference_scenario_0 <- world_reference_scenario_0$SE
se_act <- diseasy::DiseasyActivity$new(contact_basis = diseasy::contact_basis$SE,
                                       activity_units = diseasy::dk_activity_units)
se_act$change_activity(utils::head(se_reference_scenario_0, 1))

se_act$scenario_matrix

risk_df <- se_reference_scenario_0 |>
  dplyr::select("date", "risk") |>
  dplyr::cross_join(data.frame(type = c("home", "work", "school", "other")))
se_act$change_risk(risk_df$date, risk_df$type, risk_df$risk)

se_act$risk_matrix

se_scenario_freedom <- se_act$get_scenario_openness(age_cuts_lower = 0)

purrr::map2(se_scenario_freedom, names(se_scenario_freedom), ~ {
    tibble::enframe(.x, name = "arena", value = "freedom") |>
      dplyr::transmute("date" = as.Date(.y), .data$arena, "freedom" = unlist(.data$freedom))
  }) |>
  purrr::reduce(dplyr::union_all) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = freedom, color = arena)) +
  ggplot2::geom_line(size = 1)







# Configure a Swedish activity scenario
se_reference_scenario_1 <- world_reference_scenario_1$SE
se_act <- diseasy::DiseasyActivity$new(contact_basis = diseasy::contact_basis$SE,
                                       activity_units = diseasy::dk_activity_units)
se_act$change_activity(utils::head(se_reference_scenario_1, 1))

se_act$scenario_matrix

risk_df <- se_reference_scenario_1 |>
  dplyr::select("date", tidyselect::starts_with("risk")) |>
  tidyr::pivot_longer(!"date", names_to = "type", names_pattern = r"{risk_(\w*)}",
                      values_to = "risk", values_transform = as.numeric)
se_act$change_risk(risk_df$date, risk_df$type, risk_df$risk)

se_act$risk_matrix

se_scenario_freedom <- se_act$get_scenario_openness(age_cuts_lower = 0)

purrr::map2(se_scenario_freedom, names(se_scenario_freedom), ~ {
  tibble::enframe(.x, name = "arena", value = "freedom") |>
    dplyr::transmute("date" = as.Date(.y), .data$arena, "freedom" = unlist(.data$freedom))
}) |>
  purrr::reduce(dplyr::union_all) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = freedom, color = arena)) +
  ggplot2::geom_line(size = 1)
