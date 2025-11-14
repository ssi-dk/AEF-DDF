# TODOs
# Should we set individual_level = FALSE and / or monotonous = FALSE?

# Controls for the outputs

# Set figure targets
single_target <- "sigmoidal_waning"
M_subset <- c(2, 6, 10)

# Set optimiser parameters
monotonous <- FALSE
individual_level <- FALSE

# Controls for the computation
future::plan("multicore", gc = TRUE, workers = unname(future::availableCores(omit = 1)))




# Lockfile should only be created when getting a new version of diseasy
## pak::lockfile_create("ssi-dk/diseasy", lib = "r-library", upgrade = TRUE)
## pak::pak("ssi-dk/diseasy", lib = "r-library")

# To run the script, please install the dependencies from the lockfile
# unlink("r-library/", recursive = TRUE)
# pak::lockfile_install(lib = "r-library", update = TRUE)

# Determine the installed packages and R version
installation_state <- c(
  "R" = digest::digest(version),
  "packages" = digest::digest(readLines("pkg.lock"))
)

# Setup a cache for the analysis
cache <- cachem::cache_disk(dir = "cache/")
if (!identical(cache$get("installation_state"), installation_state)) {
  #cache$reset()
  cache$set("installation_state", installation_state)
}
options("diseasy.cache" = cache)

# Set R to use the specific packages defined in the lockfile
#.libPaths(file.path(getwd(), "r-library"))
#stopifnot(endsWith(.libPaths()[1], "r-library"))



library(diseasy)


## First figure
# For our first figure, we want to show the performance of the different methods in approximating some simple target curves
# as the number of compartments increase.

# For each of the three methods, we want to show their approximation for M = 1, 3, and 5 against a sigmoidal target
# and a sum of exponentials.


# Define custom waning functions
single_target_waning_functions <- list(
  "sigmoidal_waning" = \(t) exp(-(t - 1) * 6) / (1 + exp(-(t - 1) * 6)),
  "sum_exp" = \(t) 1 / 2 * (exp(- t / 2) + exp(-2 * t)),
  "exponential" = \(t) exp(-t)
)

# Determine optimal optimisers per method/strategy pair
optim_control <- diseasy::diseasy_immunity_optimiser_results |>
  dplyr::group_by(dplyr::across(c("method", "strategy", "optim_method"))) |>
  dplyr::summarise(
    "value" = sum(.data$value, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::filter(.data$value > 0) |>
  dplyr::slice_min(.data$value, with_ties = FALSE) |>
  dplyr::mutate(
    "optim_control" = purrr::map(.data$optim_method, ~ list("optim_method" = .))
  ) |>
  dplyr::select(!c("value", "optim_method"))


# Get combinations of problems to solve
inputs <- tidyr::expand_grid(
  "target" = names(single_target_waning_functions),
  "method-strategy" = c(
    "free_gamma-naive", "free_gamma-recursive",
    "free_delta-naive", "free_delta-recursive",
    "all_free-naive", "all_free-recursive", "all_free-combination"
  ),
  "M" = seq.int(from = 1, to = max(M_subset), by = 1)
) |>
  tidyr::separate_wider_delim(
    cols = "method-strategy",
    delim = "-",
    names = c("method", "strategy")
  ) |>
  dplyr::left_join(
    tibble::enframe(
      single_target_waning_functions,
      name = "target",
      value = "waning_function"
    ),
    by = "target"
  ) |>
  dplyr::left_join(
    optim_control,
    by = c("method", "strategy")
  )

# Generate approximations (stored in the cache)
approximation_output <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs,
  \(target, method, strategy, M, waning_function, optim_control) {

    try({
      im <- DiseasyImmunity$new()

      im$set_custom_waning(
        custom_function = waning_function,
        target = "infection",
        name = target,
      )

      approx <- im$approximate_compartmental(
        method = method,
        M = M,
        strategy = strategy,
        monotonous = monotonous,
        individual_level = individual_level
      )

      # Get a reference to the internal helper functions
      private <- im$.__enclos_env__$private

      # Convert gamma and delta values to plotting functions
      modifyList(
        approx,
        list(
          "target" = target,
          "approx_function" = private$get_approximation(approx$gamma$infection, approx$delta, M)
        )
      )
    })
  }
)


# Generate data.frame for plotting
generators <- cbind(
  inputs,
  tibble::tibble("approx_function" = approximation_output$approx_function)
) |>
  tibble::as_tibble()


t <- seq(0, 3.5, by = 1 / 10)
results <- generators |>
  tidyr::pivot_longer(
    c("waning_function", "approx_function"),
    names_to = "type",
    values_to = "func"
  ) |>
  tidyr::crossing(t) |>
  dplyr::mutate("y" = purrr::map2_dbl(.data$func, .data$t, \(f, t) f(t))) |>
  dplyr::select(!"func")

approx_results <- results |>
  dplyr::filter(.data$type == "approx_function")

reference_results <- results |>
  dplyr::filter(.data$type == "waning_function") |>
  dplyr::select(!c("M", "method", "strategy")) |>
  dplyr::distinct()

ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = y, color = method)) +
  ggplot2::geom_line(
    data = approx_results,
    mapping = ggplot2::aes(linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data = reference_results,
    color = "black",
    linewidth = 1
  ) +
  ggplot2::facet_grid(M ~ target) +
  ggplot2::labs(
    caption = paste(
      sep = "\n",
      "Overview of results from $approximate_compartmental",
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave("../figures/0.png")


# Compute the residuals for each method / strategy
residuals <- dplyr::left_join(
  results,
  reference_results |>
    dplyr::transmute(.data$target, .data$t, "y_ref" = .data$y),
  by = c("t", "target")
) |>
  dplyr::mutate(
    "residual" = .data$y - .data$y_ref,
    "method" = dplyr::if_else(.data$type == "waning_function", "Target", .data$method)
  ) |>
  tidyr::pivot_longer(c("y", "residual")) |>
  dplyr::distinct() |>
  dplyr::mutate("name" = factor(.data$name, levels = c("y", "residual"), labels = c("y", "Residual")))


# Plot a subset of the data for illustrative purposes
ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      residuals,
      .data$target == !!single_target,
      .data$M %in% M_subset,
      .data$method != "Target"
    ),
    mapping = ggplot2::aes(x = t, y = value, colour = method, linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(
      residuals,
      .data$target == !!single_target,
      .data$M %in% M_subset,
      .data$method == "Target",
      .data$strategy == "combination"
    ),
    mapping = ggplot2::aes(x = t, y = value, colour = method),
    linewidth = 1
  ) +
  ggplot2::facet_grid(
    name ~ M,
    scales = "free",
    labeller = ggplot2::labeller(M = ggplot2::as_labeller(\(M) paste("M =", M)))
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      title = "Method"
    ),
    linetype = ggplot2::guide_legend(
      title = "Strategy"
    )
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Target"     = "#7C8695", # SSI Gray
      "all_free"   = "#B51412", # SSI Red
      "free_gamma" = "#367F68", # SSI Green
      "free_delta" = "#3C6088"  # SSI Blue
    )
  ) +
  ggplot2::labs(y = "") +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    caption = paste(
      sep = "\n",
      "Focused presentation of results from $approximate_compartmental",
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave("../figures/1.png")




# Create an elbow curve of total residuals
error <- cbind(
  inputs,
  tibble::tibble("error" = approximation_output$sqrt_integral)
) |>
  dplyr::summarise(
    "error" = sum(.data$error),
    .by = c("M", "method", "strategy")
  )


# Plot a subset of the data for illustrative purposes
ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      error,
      .data$M > 1
    ),
    mapping = ggplot2::aes(x = M, y = error, colour = method, linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      title = "Method"
    ),
    linetype = ggplot2::guide_legend(
      title = "Strategy"
    )
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412", # SSI Red
      "free_gamma" = "#367F68", # SSI Green
      "free_delta" = "#3C6088"  # SSI Blue
    )
  ) +
  ggplot2::labs(y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}")) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    caption = paste(
      sep = "\n",
      glue::glue("Error (disregarding penalty) from approximation to single targets"),
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave("../figures/2.png")



# Define custom waning functions for dual waning optimisation
dual_target_waning_functions <- list(
  "sigmoidal_waning" = \(t) exp(-(t - 1) * 6) / (1 + exp(-(t - 1) * 6)),
  "sum_exp" = \(t) 1 / 2 * (exp(- t / 2) + exp(-2 * t)),
  "exponential" = \(t) exp(-t),
  "sigmoidal_waning_2t" = \(t) exp(-((t / 2) - 1) * 6) / (1 + exp(-((t / 2) - 1) * 6)),
  "sum_exp_2t" = \(t) 1 / 2 * (exp(- (t / 2) / 2) + exp(-2 * (t / 2))),
  "exponential_2t" = \(t) exp(-(t / 2))
)

# Get combinations of problems to solve
inputs <- tidyr::expand_grid(
  "target_1" = names(dual_target_waning_functions),
  "target_2" = names(dual_target_waning_functions),
  "method-strategy" = c(
    "free_gamma-naive", "free_gamma-recursive",
    "free_delta-naive", "free_delta-recursive",
    "all_free-naive", "all_free-recursive", "all_free-combination"
  ),
  "M" = seq.int(from = 1, to = max(M_subset), by = 1)
) |>
  tidyr::separate_wider_delim(
    cols = "method-strategy",
    delim = "-",
    names = c("method", "strategy")
  ) |>
  dplyr::left_join(
    tibble::enframe(
      dual_target_waning_functions,
      name = "target_1",
      value = "waning_function_1"
    ),
    by = "target_1"
  ) |>
  dplyr::left_join(
    tibble::enframe(
      dual_target_waning_functions,
      name = "target_2",
      value = "waning_function_2"
    ),
    by = "target_2"
  ) |>
  dplyr::left_join(
    optim_control,
    by = c("method", "strategy")
  )


# Test only a single combination of the two targets (permutation does not matter)
inputs <- inputs |>
  dplyr::filter(.data$target_1 != .data$target_2) |>
  dplyr::mutate(
    "target_set" = purrr::map2_chr(
      .x = .data$target_1,
      .y = .data$target_2,
      ~ toString(sort(c(.x, .y)))
    )
  ) |>
  dplyr::slice_head(
    n = 1,
    by = c("method", "strategy", "M", "target_set")
  ) |>
  dplyr::select(!"target_set")



approximation_output <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs,
  \(target_1, target_2, method, strategy, M, waning_function_1, waning_function_2, optim_control) {

    try({
      im <- DiseasyImmunity$new()

      im$set_custom_waning(
        custom_function = waning_function_1,
        target = "target_1",
        name = target_1,
      )

      im$set_custom_waning(
        custom_function = waning_function_2,
        target = "target_2",
        name = target_2,
      )

      approx <- im$approximate_compartmental(
        method = method,
        M = M,
        strategy = strategy,
        monotonous = monotonous,
        individual_level = individual_level
      )

      # Get a reference to the internal helper functions
      private <- im$.__enclos_env__$private

      modifyList(
        approx,
        list(
          "target_1" = target_1,
          "target_2" = target_2,
          "approx_function_1" = private$get_approximation(approx$gamma$target_1, approx$delta, M),
          "approx_function_2" = private$get_approximation(approx$gamma$target_2, approx$delta, M)
        )
      )
    })
  }
)


# Create an elbow curve of total residuals
error <- cbind(
  inputs,
  tibble::tibble("error" = approximation_output$sqrt_integral)
) |>
  dplyr::summarise(
    "error" = sum(.data$error),
    .by = c("M", "method", "strategy")
  )


# Plot a subset of the data for illustrative purposes
ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      error,
      .data$M > 1
    ),
    mapping = ggplot2::aes(x = M, y = error, colour = method, linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      title = "Method"
    ),
    linetype = ggplot2::guide_legend(
      title = "Strategy"
    )
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412", # SSI Red
      "free_gamma" = "#367F68", # SSI Green
      "free_delta" = "#3C6088"  # SSI Blue
    )
  ) +
  ggplot2::labs(y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}")) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    caption = paste(
      sep = "\n",
      glue::glue("Error (disregarding penalty) from approximation to two targets"),
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave("../figures/3.png")
