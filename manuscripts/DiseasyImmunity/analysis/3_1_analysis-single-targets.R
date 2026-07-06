# Get figure targets
M_single <- options("analysis.M_single")
M_double <- options("analysis.M_double")

# Get optimiser parameters
monotonous <- options("analysis.monotonous")
individual_level <- options("analysis.individual_level")

# Load diseasy package
library(diseasy)

# Define custom waning functions to form the basis for the figures
single_target_waning_functions <- list(
  "Exponential" = \(t) exp(-t),
  "Sum of exponentials" = \(t) 1 / 2 * (exp(- t / 2) + exp(-2 * t)),
  "Sigmoidal" = \(t) exp(-(t - 1) * 6) / (1 + exp(-(t - 1) * 6))
)

# Determine optimal optimisers per method/strategy pair
optim_control <- diseasy::diseasy_immunity_optimiser_results |>
  dplyr::filter(
    .data$penalty == !!(monotonous | individual_level), # Match analysis config.
    .data$variation == "Base"
  ) |>
  dplyr::group_by(
    dplyr::across(c("method", "strategy", "M", "optim_method"))
  ) |>
  dplyr::summarise(
    "value" = sum(.data$value, na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  dplyr::filter(.data$value >= 0) |>
  dplyr::slice_min(.data$value, with_ties = FALSE) |>
  tidyr::separate(
    col = "optim_method",
    into = c("optim_method", "localsolver"),
    sep = "_",
    fill = "right"
  ) |>
  dplyr::mutate(
    "optim_control" = purrr::map2(
      .data$optim_method,
      .data$localsolver,
      ~ purrr::discard(
        list("optim_method" = .x, "localsolver" = toupper(.y)),
        is.na
      )
    )
  ) |>
  dplyr::select(!c("value", "optim_method", "localsolver"))


# Get combinations of problems to solve
inputs_single <- tidyr::expand_grid(
  "target" = names(single_target_waning_functions),
  "method-strategy" = c(
    "free_gamma-naive", "free_gamma-recursive",
    "free_delta-naive", "free_delta-recursive",
    "all_free-naive", "all_free-recursive", "all_free-combination"
  ),
  "defaults" = c(TRUE, FALSE),
  "M" = seq.int(from = 1, to = max(M_single), by = 1),
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
    by = c("method", "strategy", "M")
  )

# Generate approximations (stored in the cache)
outputs_single <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs_single,
  \(target, method, strategy, defaults, M, waning_function, optim_control) {

    try({
      im <- DiseasyImmunity$new()

      im$set_custom_waning(
        custom_function = waning_function,
        target = "infection",
        name = target,
      )

      # Override optim_control
      if (defaults) optim_control <- NULL

      approx <- im$approximate_compartmental(
        method = method,
        M = M,
        strategy = strategy,
        monotonous = monotonous,
        individual_level = individual_level,
        optim_control = optim_control
      )

      # Get a reference to the internal helper functions
      private <- im$.__enclos_env__$private

      # Convert gamma and delta values to plotting functions
      modifyList(
        approx,
        list(
          "target" = target,
          "approx_function" = private$get_approximation(
            approx$gamma$infection,
            approx$delta,
            M
          )
        )
      )
    })
  }
)

# Convert some variables to factors to order plots
inputs_single <- inputs_single |>
  dplyr::mutate(
    "target" = factor(
      .data$target,
      levels = names(single_target_waning_functions)
    ),
    "method" = factor(
      .data$method,
      levels = c("free_delta", "free_gamma", "all_free")
    ),
    "strategy" = factor(
      .data$strategy,
      levels = c("recursive", "naive", "combination")
    ),
    "optim_control" = dplyr::if_else(
      .data$defaults, list("optim_method" = "ucminf"), .data$optim_control
    ),
    "defaults" = factor(
      .data$defaults,
      levels = c(TRUE, FALSE),
      labels = c("Default optimiser", "Smallest error optimiser")
    )
  )


# Generate data.frame for plotting
generators_single <- cbind(
  inputs_single,
  tibble::tibble(
    "approx_function" = purrr::map(outputs_single, ~ .$approx_function)
  )
) |>
  tibble::as_tibble() |>
  dplyr::filter(.data$defaults == "Default optimiser")


t <- seq(0, 3.5, by = 1 / 20)
results_single <- generators_single |>
  tidyr::pivot_longer(
    c("waning_function", "approx_function"),
    names_to = "type",
    values_to = "func"
  ) |>
  tidyr::crossing(t) |>
  dplyr::mutate("y" = purrr::map2_dbl(.data$func, .data$t, \(f, t) f(t))) |>
  dplyr::select(!"func")


#  __        ___  __          ___          __        __  ___
# /  \ \  / |__  |__) \  / | |__  |  |    |__) |    /  \  |
# \__/  \/  |___ |  \  \/  | |___ |/\|    |    |___ \__/  |

approx_results_single <- results_single |>
  dplyr::filter(.data$type == "approx_function")

reference_results_single <- results_single |>
  dplyr::filter(.data$type == "waning_function") |>
  dplyr::select(!c("M", "method", "strategy")) |>
  dplyr::distinct()

g <- ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = y, color = method)) +
  ggplot2::geom_line(
    data = dplyr::filter(approx_results_single, .data$M < 4),
    mapping = ggplot2::aes(linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::geom_line(
    data = reference_results_single,
    color = "black",
    linewidth = 1
  ) +
  ggplot2::facet_grid(M ~ target) +
  ggplot2::labs(
    caption = paste(
      sep = "\n",
      "Overview of results from $approximate_compartmental()",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-overview.png"),
  plot = g
)

#         __            __                     __        __  ___  __
# | |\ | |  \ | \  / | |  \ |  |  /\  |       |__) |    /  \  |  /__`
# | | \| |__/ |  \/  | |__/ \__/ /~~\ |___    |    |___ \__/  |  .__/

# Compute the residuals for each method / strategy
residuals_single <- dplyr::left_join(
  results_single,
  reference_results_single |>
    dplyr::transmute(.data$target, .data$t, "y_ref" = .data$y),
  by = c("t", "target"),
  relationship = "many-to-many"
) |>
  dplyr::mutate(
    "residual" = .data$y - .data$y_ref,
    "method" = dplyr::if_else(
      .data$type == "waning_function",
      "Target",
      .data$method
    )
  ) |>
  tidyr::pivot_longer(c("y", "residual")) |>
  dplyr::distinct() |>
  dplyr::mutate(
    "name" = factor(
      .data$name,
      levels = c("y", "residual"),
      labels = c("y", "Residual")
    )
  )



# Plot a subset of the data for illustrative purposes
for (single_target in names(single_target_waning_functions)) {
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(
        residuals_single,
        .data$target == !!single_target,
        .data$M %in% M_single,
        .data$method == "Target",
        .data$strategy == "combination"
      ),
      mapping = ggplot2::aes(x = t, y = value),
      linewidth = 1,
      colour = "#7C8695"
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(
        residuals_single,
        .data$target == !!single_target,
        .data$M %in% M_single,
        .data$method != "Target"
      ),
      mapping = ggplot2::aes(
        x = t,
        y = value,
        colour = method,
        linetype = strategy
      ),
      linewidth = 1
    ) +
    ggplot2::facet_grid(
      name ~ M,
      scales = "free",
      labeller = ggplot2::labeller(
        M = ggplot2::as_labeller(\(M) paste("M =", M))
      )
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = "Method", order = 1),
      linetype = ggplot2::guide_legend(title = "Strategy", order  = 2)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "all_free"   = "#B51412",
        "free_gamma" = "#367F68",
        "free_delta" = "#3C6088"
      ),
      breaks = c("Target", "free_delta", "free_gamma", "all_free")
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = "",
      title = paste("Approximation to", single_target),
      caption = paste(
        sep = "\n",
        glue::glue("individual_level = {individual_level}"),
        glue::glue("monotonous = {monotonous}")
      )
    )


  label <- stringr::str_replace_all(tolower(single_target), " ", "-")
  ggplot2::ggsave(
    file.path(
      dirname(getwd()),
      glue::glue("figures/single-target-{label}.png")
    ),
    plot = g
  )

  # Show the plot
  if (interactive()) print(g)
}



# ___  __        __   ___     __   ___  ___     __        __  ___  __
#  |  |__)  /\  |  \ |__  __ /  \ |__  |__     |__) |    /  \  |  /__`
#  |  |  \ /~~\ |__/ |___    \__/ |    |       |    |___ \__/  |  .__/

# Create an elbow curve of total residuals
error_single <- inputs_single |>
  cbind(tibble::tibble("error" = purrr::map_dbl(outputs_single, ~ .$error)))

error_single_total <- error_single |>
  dplyr::summarise(
    "avg_error" = mean(.data$error),
    .by = c("M", "method", "strategy", "defaults")
  ) |>
  tibble::tibble()


g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      error_single,
      .data$M > 1,
      .data$defaults == "Default optimiser"
    ),
    mapping = ggplot2::aes(
      x = M,
      y = error,
      colour = method,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Method", order = 1),
    linetype = ggplot2::guide_legend(title = "Strategy", order = 2)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412",
      "free_gamma" = "#367F68",
      "free_delta" = "#3C6088"
    )
  ) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::facet_grid(target ~ method, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    y = "Approximation error",
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}"),
    caption = paste(
      sep = "\n",
      "Error (disregarding penalty) for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-error-per-target.png"),
  plot = g
)



g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      error_single_total,
      .data$M > 1,
      .data$defaults == "Default optimiser"
    ),
    mapping = ggplot2::aes(
      x = M,
      y = avg_error,
      colour = strategy,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Strategy"),
    linetype = ggplot2::guide_legend(title = "Strategy")
  ) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    y = "Average approximation error",
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}"),
    caption = paste(
      sep = "\n",
      "Average error (disregarding penalty) for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )


if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-error-total.png"),
  plot = g
)



# Create an elbow curve of total residuals
execution_time_single <- inputs_single |>
  cbind(
    tibble::tibble(
      "execution_time" = purrr::map_dbl(
        outputs_single,
        ~ as.numeric(.$execution_time, "minutes")
      )
    )
  )

execution_time_single_total <- execution_time_single |>
  dplyr::summarise(
    "avg_execution_time" = mean(.data$execution_time),
    .by = c("M", "method", "strategy", "defaults")
  ) |>
  tibble::tibble()


g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      execution_time_single,
      .data$M > 1,
      .data$defaults == "Default optimiser"
    ),
    mapping = ggplot2::aes(
      x = M,
      y = execution_time,
      colour = method,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "method", order = 1),
    linetype = ggplot2::guide_legend(title = "Strategy", order  = 2)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412",
      "free_gamma" = "#367F68",
      "free_delta" = "#3C6088"
    )
  ) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::scale_y_log10() +
  ggplot2::facet_grid(target ~ method) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    y = "Average execution time (minutes)",
    caption = paste(
      sep = "\n",
      "Execution time for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )


if (interactive()) print(g)
ggplot2::ggsave(
  file.path(
    dirname(getwd()),
    "figures/single-target-execution-time-per-target.png"
  ),
  plot = g
)



g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      execution_time_single_total,
      .data$M > 1
    ),
    mapping = ggplot2::aes(
      x = M,
      y = avg_execution_time,
      colour = strategy,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Strategy"),
    linetype = ggplot2::guide_legend(title = "Strategy")
  ) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::facet_grid(defaults ~ method) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    y = "Average execution time (minutes)",
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}"),
    caption = paste(
      sep = "\n",
      "Average execution time for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-execution-time-total.png"),
  plot = g
)




trade_off_single_total <- dplyr::full_join(
  error_single_total,
  execution_time_single_total,
  by = c("M", "method", "strategy", "defaults")
) |>
  tidyr::pivot_longer(c("avg_error", "avg_execution_time")) |>
  dplyr::mutate(
    "name" = dplyr::if_else(
      .data$name == "avg_error",
      "Average approximation error",
      "Average execution time (minutes)"
    )
  )


g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      trade_off_single_total,
      .data$M > 1,
      .data$defaults == "Default optimiser"
    ),
    mapping = ggplot2::aes(
      x = M,
      y = value,
      colour = method,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Method", order = 1),
    linetype = ggplot2::guide_legend(title = "Strategy", order = 2)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412",
      "free_gamma" = "#367F68",
      "free_delta" = "#3C6088"
    )
  ) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::facet_wrap(~ name, ncol = 1, scales = "free") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    y = NULL,
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}")
    caption = paste(
      sep = "\n",
      "Error (disregarding penalty) and execution time for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-trade-off-total.png"),
  plot = g
)




g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      trade_off_single_total,
      .data$M > 1
    ),
    mapping = ggplot2::aes(
      x = M,
      y = value,
      colour = method,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Method", order = 1),
    linetype = ggplot2::guide_legend(title = "Strategy", order = 2)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "all_free"   = "#B51412",
      "free_gamma" = "#367F68",
      "free_delta" = "#3C6088"
    )
  ) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::facet_grid(
    name ~ defaults,
    scales = "free"
  ) +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    y = NULL,
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}")
    caption = paste(
      sep = "\n",
      "Error (disregarding penalty) and execution time for single targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)

ggplot2::ggsave(
  file.path(
    dirname(getwd()),
    "figures/single-target-trade-off-optim-choice.png"
  ),
  plot = g
)


dplyr::full_join(
  error_single_total,
  execution_time_single_total,
  by = c("M", "method", "strategy", "defaults")
) |>
  dplyr::mutate("defaults" = .data$defaults == "Default optimiser") |>
  tidyr::pivot_wider(
    id_cols = c("M", "method", "strategy"),
    names_from = "defaults",
    values_from = c("avg_error", "avg_execution_time")
  ) |>
  dplyr::transmute(
    .data$M, .data$method, .data$strategy,
    "diff_error" = .data$avg_error_FALSE - .data$avg_error_TRUE,
    "diff_execution_time" = .data$avg_execution_time_FALSE - .data$avg_execution_time_TRUE
  ) |>
  dplyr::summarise(
    "diff_error" = sum(.data$diff_error),
    "diff_execution_time" = sum(.data$diff_execution_time),
    .by = c("method", "strategy")
  )


#  __   ___                ___         __        __  ___  __
# |__) |__  |\ |  /\  |     |  \ /    |__) |    /  \  |  /__`
# |    |___ | \| /~~\ |___  |   |     |    |___ \__/  |  .__/

# Plot the distributution of parameters
# Create an elbow curve of total residuals
parameters <- inputs_single |>
  cbind(
    tibble::tibble(
      "gamma" = purrr::map(outputs_single, ~ .$gamma$infection),
      "delta" = purrr::map(outputs_single, ~ .$delta)
    )
  ) |>
  dplyr::filter(
    .data$M > 1,
    .data$defaults == "Default optimiser"
  ) |>
  tibble::as_tibble()

parameters <- parameters |>
  dplyr::mutate(
    "non_monotonicity" = purrr::map_dbl(
      .data$gamma,
      \(gamma) sum(purrr::keep(diff(gamma), ~ . > 0))
    ),
    "gamma_penalty" = purrr::pmap_dbl(
      dplyr::select(parameters, gamma, M, waning_function),
      \(gamma, M, waning_function) {
        if (length(gamma) == 1) {
          0
        } else {
          f <- waning_function
          sd(gamma) - sd(seq(from = f(0), to = f(Inf), length.out = M))
        }
      }
    ),
    "delta_penalty" = purrr::map_dbl(
      .data$delta,
      \(delta) ifelse(length(delta) > 1, sd(delta), 0)
    ),
    "variation_penalty" = .data$gamma_penalty + .data$delta_penalty
  )


g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = parameters |>
      dplyr::select(!c("waning_function", "optim_control", "defaults")) |>
      tidyr::pivot_longer(
        cols = c("non_monotonicity", "gamma_penalty", "delta_penalty")
      ),
    mapping = ggplot2::aes(
      x = M,
      y = value,
      colour = name,
      linetype = strategy
    ),
    linewidth = 1
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(title = "Strategy"),
    linetype = ggplot2::guide_legend(title = "Strategy")
  ) +
  ggplot2::coord_cartesian(ylim = c(0, NA), expand = FALSE) +
  ggplot2::facet_grid(method ~ target, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.spacing = ggplot2::unit(1, "lines")) +
  ggplot2::labs(
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}"),
    caption = paste(
      sep = "\n",
      glue::glue("Penalties ({ifelse(individual_level, 'applied', 'not applied')}) for single targets"),
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/single-target-penalty-per-target.png"),
  plot = g
)
