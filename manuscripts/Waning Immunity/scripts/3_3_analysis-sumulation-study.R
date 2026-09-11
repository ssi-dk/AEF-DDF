# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "Waning Immunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)



# Load diseasy package
withr::local_options("diseasy.logging" = FALSE)
withr::local_seed(4260)

# Testing space
Ms <- seq.int(from = 1, to = 10) # Increasing # of R compartments
waning_functions <- list(
  "exponential" = \(t) exp(-t / time_scale),
  "sigmoidal" = \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "exp_sum" = \(t) (exp(-0.5 * t / time_scale) + exp(-2 * t / time_scale)) / 2
)

tests <- purrr::list_rbind(
  list(
    tidyr::expand_grid(
      "infection_waning" = waning_functions,
    ),
    tidyr::expand_grid(
      "infection_waning" = waning_functions,
      "hospitalisation_waning" = waning_functions,
    )
  )
) |>
  dplyr::cross_join(
    tidyr::expand_grid(
      "M" = Ms,
      "relative_time_scale" = 1
    )
  )

model_rates <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  tests,
  \(infection_waning, hospitalisation_waning, M, relative_time_scale) {

    # Configure immunity module
    time_scale <- 180
    immunity <- diseasy::DiseasyImmunity$new()
    immunity$set_custom_waning(
      custom_function = infection_waning,
      time_scale = time_scale
    )
    if (!is.null(hospitalisation_waning)) {
      immunity$set_custom_waning(
        custom_function = hospitalisation_waning,
        target = "hospitalisation",
        time_scale = time_scale * relative_time_scale
      )
    }

    # Create ODE instances
    model <- diseasy:::generate_example_seir_model(
      module_overrides = list("immunity" = immunity),
      parameter_overrides = list("compartment_structure" = c("E" = 2L, "I" = 1L, "R" = M))
    )

    # Get a reference to the private environment
    private <- model$.__enclos_env__$private

    age_cuts_lower <- model$population$age_cuts_lower

    population_proportion <- model$activity$map_population(age_cuts_lower) |>
      dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group_out") |>
      dplyr::pull("proportion")

    activity_proportion <- cbind(
      model$activity$map_population(age_cuts_lower) |>
        dplyr::summarise(
          "proportion" = sum(.data$proportion),
          .by = c("age_group_reference", "age_group_out")
        ),
      "activity" = rowSums(model$activity$get_scenario_contacts(weights = c(1, 1, 1, 1))[[1]])
    ) |>
      dplyr::summarise("activity" = sum(.data$activity), .by = "age_group_out") |>
      dplyr::pull("activity")

    activity <- population_proportion * activity_proportion
    activity <- activity / sum(activity)

    y0 <- rep(0, private$n_states)

    # 0.05% are newly infected
    y0[private$e1_state_indices] <- activity * 0.0005

    # 99.95% are susceptible
    y0[private$s_state_indices] <- population_proportion - y0[private$e1_state_indices]

    # Run solver across scenario change to check for long-term leakage
    sol <- deSolve::ode(
      y = y0,
      times = seq(0, 3 * time_scale),
      func = model$rhs
    )

    # Improve the names of the output
    states <- list(
      "EIR" = purrr::imap(
        model$parameters$compartment_structure,
        ~ paste0(rep(.y, .x), seq_len(.x))
      ) |>
        purrr::reduce(c),
      "S" = "S"
    )

    state_labels <- purrr::map(
      states,
      ~ {
        tidyr::expand_grid(
          "variant" = purrr::pluck(model, "variant", "variants", .default = NA_character_),
          model$population$groups,
          "state" = .x
        )
      }
    ) |>
      purrr::list_rbind() |>
      tidyr::unite(
        "label",
        "variant", names(model$population$groups), "state",
        sep = "/",
        na.rm = FALSE,
        remove = FALSE
      )

    colnames(sol) <- c("time", dplyr::pull(state_labels, "label"))

    # Convert to long format
    sol_long <- sol |>
      as.data.frame() |>
      tidyr::pivot_longer(
        !"time",
        names_sep = "/",
        names_to = colnames(dplyr::select(state_labels, !"label"))
      )

    # Extract rates for the I1-exit (= n_infected) and each configured
    # observable.
    # Custom model outputs computes from differences of integrating states
    model_rates <- sol_long |>
      dplyr::filter(.data$state %in% c("I1", model$model_outputs)) |>
      dplyr::mutate(
        "rate" = dplyr::if_else(
          .data$state == "I1",
          model$parameters$disease_progression_rates[["I"]] *
            model$parameters$compartment_structure[["I"]] * .data$value,
          dplyr::lead(.data$value, order_by = .data$time) - .data$value
        ),
        "outcome" = dplyr::if_else(
          .data$state == "I1",
          "n_infected",
          .data$state
        ),
        .by = !c("time", "value")
      ) |>
      dplyr::select(!"value")

    return(model_rates)
  }
)

ggplot2::ggplot(
  data = model_rates[[1]],
  mapping = ggplot2::aes(x = time, y = rate, colour = age_group)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ outcome)


results <- tests |>
  dplyr::mutate(
    "metric" = purrr::map_dbl(
      model_rates,
      ~ sum(.x$rate)
    ),
    "type" = dplyr::if_else(
      is.null(.data$hospitalisation_waning),
      "Single outcome",
      "Double outcome"
    )
  )
