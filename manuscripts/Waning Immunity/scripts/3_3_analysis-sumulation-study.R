# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "DiseasyImmunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)



# Load diseasy package
library(diseasy)
withr::local_options("diseasy.logging" = FALSE)
withr::local_seed(4260)

# Set the time scales of the problem
rE <- 1 / 2.1
rI <- 1 / 4.5

overall_infection_risk <- 1e-6

# Setup the number of compartments for the generating model
K <- 2L
L <- 3L

# We need a "dummy" observables module to initialise the diseasy ODE model
# (not used for the simulation study)
obs <- DiseasyObservables$new(
  conn = DBI::dbConnect(RSQLite::SQLite()),
  last_queryable_date = Sys.Date() - 1
)


# Testing space
Ms <- seq.int(from = 1, to = 10) # Increasing # of R compartments
waning_functions <- list(
  "exponential" = \(t) exp(-t / time_scale),
  "sigmoidal" = \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "exp_sum" = \(t) (exp(-0.5 * t / time_scale) + exp(-2 * t / time_scale)) / 2
)

tests <- tidyr::expand_grid(
  M = Ms,
  waning_function = waning_functions
)

purrr::pmap(
  tests,
  \(M, waning_function) {

    M <- 1L
    waning_function <- \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6))

    time_scale <- 180

    # Configure immunity module
    #immunity <- DiseasyImmunity$new()
    #immunity$set_custom_waning(waning_function, time_scale = time_scale)


    # Define the activity scenario
    activity <- DiseasyActivity$new()
    activity$set_contact_basis(contact_basis = contact_basis_nordic %.% DK)
    activity$set_activity_units(dk_activity_units)
    activity$change_activity(date = as.Date("2020-01-01"), opening = "baseline")


    # Create ODE instances
    model <- DiseasyModelOdeSeir$new(
      observables = obs,
      activity = activity,
      #immunity = immunity,
      parameters = list(
        "compartment_structure" = c("E" = K, "I" = L, "R" = M),
        "overall_infection_risk" =  1e10,
        "disease_progression_rates" = c("E" = rE, "I" = rI)
      )
    )

    # Get a reference to the private environment
    private <- m$.__enclos_env__$private

    # Generate a initial state_vector
    y0 <- rep(0, (K + L + M + 1))

    # 0.05% are newly infected
    y0[private$e1_state_indices] <- 0.5

    # 99.95% are susceptible
    y0[private$s_state_indices] <- 1 - y0[private$e1_state_indices]

    # Run solver across scenario change to check for long-term leakage
    tt <- deSolve::ode(
      y = y0,
      times = seq(0, time_scale),
      func = m %.% rhs
    )

    # Extract the maximal test positive signal from the I1 states
    true_infected <- tt[, 1 + private$i1_state_indices] * L * rI
    data <- tibble::tibble("f_infected" = cumsum(true_infected)) |>
      dplyr::mutate("t" = dplyr::row_number(), .before = dplyr::everything())

    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = t, y = f_infected)
    ) +
      ggplot2::geom_line()



  }
)


rs_samples <- list()
f <- function(t, y, parms, ...) {
  rs_samples[t] <- list(y[private$rs_state_indices])
  print(rs_samples)
  return(y)
}

# Run solver across scenario change to check for long-term leakage
tt <- deSolve::ode(
  y = y0,
  times = seq(0, 150),
  func = m %.% rhs,
  event = list(
    time = c(0, 1, 2, 3),
    func = f
  )
)



# Unnest to develop a testing model with simple and realistic testing patterns
seir_example_data <- seir_example_data |>
  dplyr::mutate("n_infected_int" = round(.data$n_infected)) |>
  tidyr::uncount(.data$n_infected_int) |>
  dplyr::mutate(
    "p" = stats::runif(dplyr::n()), # Quenched noise
    "simple_test" = .data$p < 0.65, # Assume test percentage of 65% every day
    # Realistic testing uses a weekday effect with fewer tests in the weekend
    "realistic_test" = .data$p < 0.65 * (1.058 - floor(lubridate::wday(.data$date, week_start = 1) / 6) * 0.3)
  ) |>
  dplyr::summarise(
    "n_infected" = dplyr::first(.data$n_infected),
    "n_positive_simple" = sum(.data$simple_test),
    "n_positive" = sum(.data$realistic_test),
    .by = c("age_group", "date")
  )


# Reorder columns
seir_example_data <- seir_example_data |>
  dplyr::select("date", dplyr::everything())

# set parameters for hospitalization
risk_of_admission <- c(0.001, 0.01, 0.1) # Risk per age group
frac_to_hosp_after_days <- c(0, 0, 0.2, 0.3, 0.3, 0.1, 0.1) # must sum =1

future_admitted <- t(t(true_infected) * risk_of_admission)

admitted <- array(0, dim = dim(true_infected))

for (i in seq_len(length(frac_to_hosp_after_days))) {
  admitted <- admitted + rbind(
    array(0, dim = c(i, 3)),
    frac_to_hosp_after_days[i] * future_admitted[1: (NROW(future_admitted) - i), ]
  )
}

for (i in 1:3) admitted[, i] <- rpois(length(admitted[, i]), admitted[, i])

# Convert to long format
seir_example_data_hosp <- admitted |>
  tibble::as_tibble(rownames = "t") |>
  tidyr::pivot_longer(cols = !"t", names_to = "age_group", values_to = "n_admission") |>
  dplyr::mutate("date" = as.Date("2020-01-01") + as.numeric(.data$t), .after = "t") |>
  dplyr::select(!"t")

# merge data
seir_example_data <- dplyr::left_join(seir_example_data, seir_example_data_hosp, by = c("date", "age_group"))

# Visualise the example data
ggplot2::ggplot(seir_example_data) +
  ggplot2::geom_line(ggplot2::aes(x = date, y = n_infected, color = "Infected"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(x = date, y = n_positive_simple, color = "Test positive (simple)"), linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(x = date, y = n_positive, color = "Test positive (realistic)")) +
  ggplot2::geom_point(ggplot2::aes(x = date, y = 10 * n_admission, color = "Admissions * 10")) +
  ggplot2::facet_wrap(~ age_group) +
  ggplot2::ylab("Test positive / Infected / Admissions") +
  ggplot2::scale_color_manual(
    values = c("Infected" = "black", "Test positive (simple)" = "blue",
                "Test positive (realistic)" = "red", "Admissions * 10" = "darkgreen")
  )
