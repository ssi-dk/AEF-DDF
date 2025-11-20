# Set optimiser parameters
monotonous <- FALSE
individual_level <- FALSE

# Controls for the computation
future::plan("multicore", gc = TRUE, workers = unname(future::availableCores(omit = 1)))

# Setup a cache for the analysis
cache <- cachem::cache_disk(dir = "cache/")
options("diseasy.cache" = cache)

sources <- c(
  "0.0.0.9000" = "main",
  "0.0.0.9002" = "unify"
)

version <- purrr::keep_at(sources, as.character(packageVersion("diseasy")))


library(diseasy)


## First figure

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
  "method-strategy" = c("free_gamma-recursive", "free_delta-recursive", "all_free-recursive"),
  "unify" = c(TRUE, FALSE),
  "M" = 8
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
  \(target, method, strategy, unify, M, waning_function, optim_control) {

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
        individual_level = individual_level,
        unify_initial_guess = unify
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
  tibble::tibble(
    "approx_function" = purrr::map(approximation_output, ~ .$approx_function),
    "execution_time" = purrr::map_dbl(approximation_output, ~ .$execution_time)
  )
) |>
  tibble::as_tibble()

generators |>
  dplyr::summarise(sum(execution_time), .by = c("unify", "target"))

t <- seq(0, 3.5, by = 1 / 20)
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
      "Overview of results from $approximate_compartmental()",
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave(file.path(dirname(getwd()), "figures/single_target-overview.png"))


# Compute the residuals for each method / strategy
residuals <- dplyr::left_join(
  results,
  reference_results |>
    dplyr::transmute(.data$target, .data$t, "y_ref" = .data$y),
  by = c("t", "target"),
  relationship = "many-to-many"
) |>
  dplyr::mutate(
    "residual" = .data$y - .data$y_ref,
    "method" = dplyr::if_else(.data$type == "waning_function", "Target", .data$method)
  ) |>
  tidyr::pivot_longer(c("y", "residual")) |>
  dplyr::distinct() |>
  dplyr::mutate("name" = factor(.data$name, levels = c("y", "residual"), labels = c("y", "Residual"))) |>
  dplyr::mutate("version" = version)

saveRDS(residuals, glue::glue("{version}.rds"))

purrr::every(sources, ~ file.exists(glue::glue("{.}.rds")))


data <- purrr::map(sources, ~ readRDS(glue::glue("{.}.rds"))) |>
  purrr::list_rbind()

ggdata <- residuals |>
  dplyr::filter(.data$name == "Residual", .data$method != "Target") |>
  dplyr::select(target, method, strategy, M, t, value, unify) |>
  tidyr::pivot_wider(
    id_cols =  c("target", "method", "strategy", "M", "t"),
    names_from = "unify",
    values_from = "value"
  ) |>
  dplyr::mutate(diff = `FALSE` - `TRUE`)

ggdata |>
  ggplot2::ggplot(ggplot2::aes(x = t, y = diff, color = method)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~target)

ggdata |>
  dplyr::summarise(sum(diff), .by = c("method"))


data |>
  dplyr::filter(.data$name == "Residual", .data$method != "Target", .data$t == 0) |>
  dplyr::select(target, method, strategy, execution_time, version) |>
  tidyr::pivot_wider(
    id_cols =  c("target", "method", "strategy"),
    names_from = "version",
    values_from = "execution_time"
  ) |>
  dplyr::mutate(diff = main - unify)


# Plot a subset of the data for illustrative purposes
for (single_target in names(single_target_waning_functions)) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(
        residuals,
        .data$target == !!single_target,
        .data$M == 8,
        .data$method == "Target",
        .data$strategy == "combination"
      ),
      mapping = ggplot2::aes(x = t, y = value, colour = method),
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(
        residuals,
        .data$target == !!single_target,
        .data$M == 8,
        .data$method != "Target"
      ),
      mapping = ggplot2::aes(x = t, y = value, colour = method, linetype = strategy),
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
        glue::glue("Focused presentation of results for {single_target}"),
        glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
      )
    )

  ggplot2::ggsave(file.path(dirname(getwd()), glue::glue("figures/single_target-{single_target}.png")))
}
