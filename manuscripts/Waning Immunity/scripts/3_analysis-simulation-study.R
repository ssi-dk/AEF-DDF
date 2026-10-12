# Set local working dir
relative_wd <- c("AEF-DDF", "manuscripts", "Waning Immunity")
wd <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
wd <- paste(c(wd[seq_len(which(wd %in% relative_wd)[1] - 1)], relative_wd), collapse = .Platform$file.sep)
withr::local_dir(wd)



# Load diseasy package
withr::local_options("diseasy.logging" = FALSE)
withr::local_seed(4260)

# Combinations of waning functions to test
waning_functions <- list(
  "exponential" = \(t) exp(-t / time_scale),
  "sigmoidal" = \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "exp_sum" = \(t) (exp(-0.5 * t / time_scale) + exp(-2 * t / time_scale)) / 2
)

# Testing space
tests <- purrr::list_rbind(
  list(
    tibble::enframe(waning_functions, name = "infection_waning_label", value = "infection_waning"),
    dplyr::cross_join(
      tibble::enframe(waning_functions, name = "infection_waning_label", value = "infection_waning"),
      tibble::enframe(waning_functions, name = "hospitalisation_waning_label", value = "hospitalisation_waning")
    )
  )
) |>
  dplyr::cross_join(
    tidyr::expand_grid(
      "K" = c(0L, 1L, 2L),
      "L" = c(1L, 2L),
      "M" = seq.int(from = 2, to = getOption("analysis.M_simulation")), # Increasing # of R compartments
      "relative_time_scale" = c(1, 2)
    )
  )


# Generate approximations (stored in the cache)
progressr::with_progress(
  handlers = progressr::handler_progress(
    format   = ":current/:total [:bar] :percent in :elapsed ETA: :eta",
    width    = 61,
    complete = "+"
  ),

  expr = {
    p <- progressr::progressor(steps = nrow(tests))

    model_rates <- future.apply::future_lapply(
      dplyr::group_split(dplyr::group_by(tests, dplyr::row_number()), .keep = FALSE),
      future.seed = TRUE,
      FUN = \(parameters) {
        # Unlist parameters forced to lists when in data.frame form
        parameters <- purrr::map_if(parameters, is.list, ~ .[[1]])

        key <- glue::glue("ode_solution_{substr(digest::digest(diseasy::hash_environment(parameters)), 1, 8)}")
        cache <- cachem::cache_disk(dir = "diseasy-cache/", max_size = Inf)

        if (cachem::is.key_missing(cache$get(key))) {
          options("diseasy.cache" = cache)

          # Configure immunity module
          time_scale <- 180
          immunity <- diseasy::DiseasyImmunity$new()
          immunity$set_custom_waning(
            custom_function = parameters$infection_waning,
            time_scale = time_scale
          )
          if (!is.null(parameters$hospitalisation_waning)) {
            immunity$set_custom_waning(
              custom_function = parameters$hospitalisation_waning,
              target = "hospitalisation",
              time_scale = time_scale * parameters$relative_time_scale
            )
          }

          # Create ODE instances
          model <- diseasy:::generate_example_seir_model(
            module_overrides = list("immunity" = immunity),
            parameter_overrides = list(
              "compartment_structure" = c("E" = parameters$K, "I" = parameters$L, "R" = parameters$M)
            )
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

          y0 <- rep(0, length(private$progression_flow_rates))

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

          if (!is.null(parameters$hospitalisation_waning)) {
            states <- c(states, "n_hospitalisation" = "n_hospitalisation")
          }

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
                .data$value - dplyr::lag(.data$value, order_by = .data$time)
              ),
              "outcome" = dplyr::if_else(
                .data$state == "I1",
                "n_infected",
                .data$state
              ),
              .by = !c("time", "value")
            ) |>
            dplyr::select(!c("value", "state"))


          # Add parameters for the run
          model_rates <- dplyr::cross_join(
            model_rates,
            tidyr::expand_grid(!!!purrr::map_if(parameters, is.function, ~ list(.)))
          )

          # Add the approximation error for the run
          approx <- immunity$approximate_compartmental(M = parameters$M)

          model_rates <- dplyr::cross_join(
            model_rates,
            tidyr::expand_grid(!!!purrr::keep_at(approx, c("error", "penalty", "execution_time", "method", "strategy")))
          )

          cache$set(key, model_rates)
        }

        p()

        return(cache$get(key))

      }
    )
  }
)


ggplot2::ggplot(
  data = model_rates[[1]],
  mapping = ggplot2::aes(x = time, y = rate, colour = age_group)
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ outcome)


results <- model_rates |>
  purrr::list_rbind() |>
  dplyr::summarise(
    "metric" = sum(.data$rate, na.rm = TRUE),
    "error" = unique(.data$error),
    .by = c(colnames(tests), "outcome")
  ) |>
  tidyr::unite(
    "uuid",
    colnames(dplyr::select(tests, !"M")),
    remove = FALSE
  ) |>
  dplyr::mutate(
    "type" = purrr::map_chr(.data$hospitalisation_waning, ~ dplyr::if_else(is.null(.), "Single outcome", "Double outcome"))
  ) |>
  dplyr::mutate(
    "uuid" = purrr::map_chr(.data$uuid, digest::digest),
    .after = dplyr::everything()
  )


results <- dplyr::group_modify(
  dplyr::group_by(results, .data$uuid, .data$outcome),
  ~ dplyr::mutate(.x, "metric_min" = dplyr::slice_min(.x, .data$error)$metric)
) |>
  dplyr::mutate("relative_metric" = .data$metric / .data$metric_min)


# Collapse labels
results <- results |>
  dplyr::mutate(
    "outcome" = dplyr::case_when(
      .data$outcome == "n_infected" ~ "n_infected",
      .data$relative_time_scale > 1 ~ paste0(.data$outcome, " (tau = ", .data$relative_time_scale, ")"),
      TRUE ~ .data$outcome
    )
  )

g <- ggplot2::ggplot(
  data = results,
  mapping = ggplot2::aes(x = .data$error, y = .data$relative_metric, group = .data$uuid, colour = factor(.data$M))
) +
  #ggplot2::geom_hline(
  #  mapping = ggplot2::aes(yintercept = metric),
  #  data = dplyr::slice_min(results, .data$error, by = "outcome"),
  #  linetype = "dashed"
  #) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_x_log10() +
  #ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "M", order = 1)
  ) +
  ggplot2::facet_grid(.data$type ~ outcome)

if (interactive()) print(g)
ggplot2::ggsave(
  create.dir = TRUE,
  file.path(
    dirname(getwd()),
    "Waning Immunity",
    "figures",
    "simulation_study.png"
  ),
  plot = g
)
