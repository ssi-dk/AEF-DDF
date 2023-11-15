# Get the Oxford COVID-19 Government Restriction Tracker index for stringency
oxford_data <- readr::read_csv(
  "https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_nat_latest.csv",
  col_types = list("Date" = "character"),
  show_col_types = FALSE
)

stringency_data <- oxford_data |>
  dplyr::filter(.data$Jurisdiction == "NAT_TOTAL") |>
  dplyr::transmute(
    "date" = as.Date(Date, "%Y%m%d"),
    "country_name" = .data$CountryName,
    "country_code" = countrycode::countrycode(.data$CountryName, origin = "country.name", destination = "iso2c"),
    "stringency" = .data$StringencyIndex_Average
  )



# Some countries have stringency changes before "2020-01-01"
# Must note that in the description of the data set
stringency_data |>
  dplyr::group_by(country_code) |>
  dplyr::slice_min(date) |>
  dplyr::filter(stringency > 0)


# Store only changes
stringency_changes <- stringency_data |>
  dplyr::filter(.data$stringency != dplyr::lag(.data$stringency) | (dplyr::row_number() == 1 & .data$stringency > 0))

# Calibrate using Danish data
dk_act <- diseasy::DiseasyActivity$new(activity_units = diseasy::dk_activity_units,
                                       contact_basis = diseasy::contact_basis$DK)
dk_act$change_activity(diseasy::dk_reference_scenario)
work_risk <- aggregate(social_distance_work ~ date, data = diseasy::dk_reference_scenario, FUN = mean)
dk_act$change_risk(date = work_risk$date, type = "work", risk = work_risk$social_distance_work)


dk_freedom <- dk_act$get_scenario_freedom(age_cuts_lower = c(0), weights = c(1,1,1,1)) |>
  tibble::enframe(name = "date", value = "freedom") |>
  dplyr::mutate(date = as.Date(date),
                freedom = unlist(freedom))

dk_stringency <- stringency_changes |>
  dplyr::filter(.data$country_code == "DK") |>
  dplyr::select("date", "stringency")

calibration_factor <- tibble::tibble(date = unique(c(dk_freedom$date, dk_stringency$date))) |>
  dplyr::left_join(dk_freedom, by = "date") |>
  dplyr::left_join(dk_stringency, by = "date") |>
  dplyr::arrange(date) |>
  dplyr::mutate(freedom = zoo::na.locf(freedom),
                stringency = zoo::na.locf(stringency),
                freedom_proxy = 1 - stringency / 100,
                k = freedom / freedom_proxy) |>
  dplyr::summarise(mean_k = mean(k)) |>
  dplyr::pull("mean_k")



world_freedom <- stringency_changes |>
  dplyr::mutate(freedom = calibration_factor * (1 - stringency / 100)) |>
  dplyr::select(!"stringency")


# Convert to reference scenarios for the world
common_country_codes <- intersect(unique(world_freedom$country_code), names(diseasy::contact_basis))

world_reference_scenario_0 <- purrr::map(common_country_codes, \(country_code) {
  world_freedom |>
    dplyr::filter(.data$country_code == !!country_code) |>
    dplyr::transmute(.data$date, "risk" = .data$freedom) |>
    dplyr::mutate("opening" = dplyr::if_else(dplyr::row_number() == 1, "baseline", NA_character_),
                  "closing" = NA_character_,,
                  .after = "date")
})

names(world_reference_scenario_0) <- common_country_codes


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


se_act$get_scenario_freedom(age_cuts_lower = 0, weights = c(1,1,1,1)) |>
  tibble::enframe(name = "date", value = "freedom") |>
  dplyr::mutate(date = as.Date(date),
                freedom = unlist(freedom)) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = freedom)) +
  ggplot2::geom_point()

