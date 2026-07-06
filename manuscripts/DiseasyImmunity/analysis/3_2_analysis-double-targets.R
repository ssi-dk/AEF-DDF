# Get figure targets
M_single <- options("analysis.M_single")
M_double <- options("analysis.M_double")

# Get optimiser parameters
monotonous <- options("analysis.monotonous")
individual_level <- options("analysis.individual_level")

# Load diseasy package
library(diseasy)

# Define custom waning functions for dual waning optimisation
dual_target_waning_functions <- list(
  "Exponential&tau = 1" = \(t) exp(-t),
  "Sum of exponentials&tau = 1" = \(t) 1 / 2 * (exp(- t / 2) + exp(-2 * t)),
  "Sigmoidal&tau = 1" = \(t) exp(-(t - 1) * 6) / (1 + exp(-(t - 1) * 6)),
  "Exponential&tau = 2" = \(t) exp(-(t / 2)),
  "Sum of exponentials&tau = 2" = \(t) 1 / 2 * (exp(- (t / 2) / 2) + exp(-2 * (t / 2))),
  "Sigmoidal&tau = 2" = \(t) exp(-((t / 2) - 1) * 6) / (1 + exp(-((t / 2) - 1) * 6))
)

# Get combinations of problems to solve
inputs_double <- tidyr::expand_grid(
  "target_1" = names(dual_target_waning_functions),
  "target_2" = names(dual_target_waning_functions),
  "method-strategy" = c(
    "free_gamma-naive", "free_gamma-recursive",
    "free_delta-naive", "free_delta-recursive",
    "all_free-naive", "all_free-recursive", "all_free-combination"
  ),
  "M" = seq.int(from = 1, to = max(M_double), by = 1)
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
  )


# Test only a single combination of the two targets (permutation does not matter)
inputs_double_subset <- inputs_double |>
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



outputs_double <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs_double_subset,
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



# Convert some variables to factors to order plots
inputs_double <- inputs_double |>
  dplyr::mutate(
    "method" = factor(
      .data$method,
      levels = c("free_delta", "free_gamma", "all_free")
    ),
    "strategy" = factor(
      .data$strategy,
      levels = c("recursive", "naive", "combination")
    )
  )

inputs_double_subset <- inputs_double_subset |>
  dplyr::mutate(
    "method" = factor(
      .data$method,
      levels = c("free_delta", "free_gamma", "all_free")
    ),
    "strategy" = factor(
      .data$strategy,
      levels = c("recursive", "naive", "combination")
    )
  )



# Generate data.frame for plotting
generators_double <- cbind(
  inputs_double_subset,
  tibble::tibble(
    "approx_function_1" = purrr::map(outputs_double, ~ .$approx_function_1),
    "approx_function_2" = purrr::map(outputs_double, ~ .$approx_function_2)
  )
) |>
  tibble::as_tibble()


t <- seq(0, 3.5, by = 1 / 20)
results_double <- generators_double |>
  tidyr::pivot_longer(
    c("waning_function_1", "waning_function_2", "approx_function_1", "approx_function_2"),
    names_to = "type",
    values_to = "func"
  ) |>
  tidyr::crossing(t) |>
  dplyr::mutate("y" = purrr::map2_dbl(.data$func, .data$t, \(f, t) f(t))) |>
  dplyr::select(!"func") |>
  dplyr::mutate(
    "target_id" = stringr::str_extract(.data$type, r"{\d}"),
    "type" = stringr::str_remove(.data$type, r"{_\d}")
  )

reference_results_double <- results_double |>
  dplyr::filter(.data$type == "waning_function") |>
  dplyr::select(!c("M", "method", "strategy")) |>
  dplyr::distinct()


# Compute the residuals for each method / strategy
residuals_double <- dplyr::left_join(
  results_double,
  reference_results_double |>
    dplyr::transmute(
      .data$target_1,
      .data$target_2,
      .data$target_id,
      .data$t,
      "y_ref" = .data$y
    ),
  by = c("t", "target_1", "target_2", "target_id"),
  relationship = "many-to-many"
) |>
  dplyr::mutate(
    "residual" = .data$y - .data$y_ref,
    "method" = factor(
      dplyr::if_else(.data$type == "waning_function", "Target", .data$method),
      levels = c(levels(.data$method), "Target")
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


#         __            __                     __        __  ___  __
# | |\ | |  \ | \  / | |  \ |  |  /\  |       |__) |    /  \  |  /__`
# | | \| |__/ |  \/  | |__/ \__/ /~~\ |___    |    |___ \__/  |  .__/

inputs_double_subset |>
  dplyr::distinct(.data$target_1, .data$target_2) |>
  dplyr::mutate("iteration_id" = dplyr::row_number()) |>
  purrr::pwalk(
    \(target_1, target_2, iteration_id) {

      g <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(
            residuals_double,
            .data$target_1 == !!target_1,
            .data$target_2 == !!target_2,
            .data$M %in% M_double,
            .data$method == "Target",
            .data$strategy == "combination"
          ) |>
            dplyr::select(!"method"),
          mapping = ggplot2::aes(x = t, y = value, group = target_id),
          linewidth = 1,
          colour = "#7C8695"
        ) +
        ggplot2::geom_line(
          data = dplyr::filter(
            residuals_double,
            .data$target_1 == !!target_1,
            .data$target_2 == !!target_2,
            .data$M %in% M_double,
            .data$method != "Target"
          ) |>
            dplyr::mutate(
              "target" = stringr::str_replace(
                dplyr::if_else(.data$target_id == 1, .data$target_1, .data$target_2),
                "&",
                " w. "
              )
            ),
          mapping = ggplot2::aes(
            x = t,
            y = value,
            colour = target,
            linetype = strategy,
            group = paste0(target_id, "&", method, "&", strategy)
          ),
          linewidth = 1
        ) +
        ggplot2::facet_grid(
          name ~ method,
          scales = "free",
          labeller = ggplot2::labeller(M = ggplot2::as_labeller(\(M) paste("M =", M)))
        ) +
        ggplot2::guides(
          colour = ggplot2::guide_legend(title = "Target", order = 1),
          linetype = ggplot2::guide_legend(title = "Strategy", order  = 2)
        ) +
        ggplot2::labs(y = "") +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          y = "",
          caption = paste(
            sep = "\n",
            "Focused presentation of results for:",
            glue::glue("{stringr::str_replace(target_1, '&', ' w. ')}"),
            glue::glue("{stringr::str_replace(target_2, '&', ' w. ')}"),
            glue::glue("M = {M_double}"),
            glue::glue("individual_level = {individual_level}"),
            glue::glue("monotonous = {monotonous}")
          )
        )

      ggplot2::ggsave(
        file.path(
          dirname(getwd()),
          glue::glue("figures/double-target-{iteration_id}.png")
        ),
        plot = g
      )

      # Show the plot
      if (interactive()) print(g)
    }
  )


# ___  __        __   ___     __   ___  ___     __        __  ___  __
#  |  |__)  /\  |  \ |__  __ /  \ |__  |__     |__) |    /  \  |  /__`
#  |  |  \ /~~\ |__/ |___    \__/ |    |       |    |___ \__/  |  .__/

# Create an elbow curve of total residuals
error_double <- inputs_double_subset |>
  cbind(tibble::tibble("error" = purrr::map_dbl(outputs_double, ~ .$error)))

error_double_overview <- error_double |>
  dplyr::summarise(
    "error" = mean(.data$error),
    .by = c("method", "strategy", "M")
  )

# Plot the error per method / target
g <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = dplyr::filter(
      error_double_overview,
      .data$M > 1
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
  ggplot2::theme_bw() +
  ggplot2::labs(
    y = "Average approximation error",
    #y = latex2exp::TeX(r"{Error $\left(\int R^2\right)$}"),
    caption = paste(
      sep = "\n",
      "Error (disregarding penalty) for two targets",
      glue::glue("individual_level = {individual_level}"),
      glue::glue("monotonous = {monotonous}")
    )
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(dirname(getwd()), "figures/double-target-error-total.png"),
  plot = g
)


error_double_gg <- inputs_double |>
  dplyr::select(c("target_1", "target_2", "method", "strategy", "M")) |>
  dplyr::left_join(
    error_double,
    by = c("target_1", "target_2", "method", "strategy", "M")
  ) |>
  dplyr::mutate(
    "target_1" = factor(
      .data$target_1,
      levels = purrr::discard(
        names(dual_target_waning_functions),
        ~ . == "Sigmoidal&tau = 2"
      ),
      labels = paste0(
        " ",
        purrr::discard(
          names(dual_target_waning_functions),
          ~ . == "Sigmoidal&tau = 2"
        )
      )
    ),
    "target_2" = factor(
      .data$target_2,
      levels = purrr::discard(
        names(dual_target_waning_functions),
        ~ . == "Exponential&tau = 1"
      ),
      labels = paste0(
        " ",
        purrr::discard(
          names(dual_target_waning_functions),
          ~ . == "Exponential&tau = 1"
        )
      )
    )
  ) |>
  dplyr::mutate(
    "added_error" = .data$error - min(.data$error),
    .by = c("target_1", "target_2", "M", "strategy")
  ) |>
  dplyr::filter(!is.na(.data$target_1) & !is.na(.data$target_2))

g <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = dplyr::filter(
      error_double_gg,
      .data$M == max(M_double),
      .data$strategy == "naive"
    ),
    mapping = ggplot2::aes(
      x = target_2,
      y = target_1,
      fill = added_error
    ),
    linewidth = 1
  ) +
  ggplot2::scale_fill_viridis_c(na.value = "white") +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(title = "Added error", order = 1),
    x = ggh4x::guide_axis_nested(delim = "&", title = ""),
    y = ggh4x::guide_axis_nested(delim = "&", title = "")
  ) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    aspect.ratio = 1,
    legend.position = "top"
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(
    dirname(getwd()),
    "figures/double-target-error-per-target.png"
  ),
  plot = g
)



# Create an elbow curve of total residuals
execution_time_double <- inputs_double_subset |>
  cbind(
    tibble::tibble(
      "execution_time" = purrr::map_dbl(
        outputs_double,
        ~ as.numeric(.$execution_time, "minutes")
      )
    )
  )


execution_time_double_gg <- inputs_double |>
  dplyr::select(c("target_1", "target_2", "method", "strategy", "M")) |>
  dplyr::left_join(
    execution_time_double,
    by = c("target_1", "target_2", "method", "strategy", "M")
  ) |>
  dplyr::mutate(
    "target_1" = factor(
      .data$target_1,
      levels = purrr::discard(
        names(dual_target_waning_functions),
        ~ . == "Sigmoidal&tau = 2"
      ),
      labels = paste0(
        " ",
        purrr::discard(
          names(dual_target_waning_functions),
          ~ . == "Sigmoidal&tau = 2"
        )
      )
    ),
    "target_2" = factor(
      .data$target_2,
      levels = purrr::discard(
        names(dual_target_waning_functions),
        ~ . == "Exponential&tau = 1"
      ),
      labels = paste0(
        " ",
        purrr::discard(
          names(dual_target_waning_functions),
          ~ . == "Exponential&tau = 1"
        )
      )
    )
  ) |>
  dplyr::mutate(
    "relative_execution_time" = (.data$execution_time - min(.data$execution_time)) / min(.data$execution_time),
    .by = c("target_1", "target_2", "M", "strategy")
  ) |>
  dplyr::filter(!is.na(.data$target_1) & !is.na(.data$target_2))

g <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = dplyr::filter(
      execution_time_double_gg,
      .data$M == max(M_double),
      .data$strategy == "naive"
    ),
    mapping = ggplot2::aes(
      x = target_2,
      y = target_1,
      fill = relative_execution_time
    ),
    linewidth = 1
  ) +
  ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "white") +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(title = "Relative execution time", order = 1),
    x = ggh4x::guide_axis_nested(delim = "&", title = ""),
    y = ggh4x::guide_axis_nested(delim = "&", title = "")
  ) +
  ggplot2::facet_wrap(~ method) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    aspect.ratio = 1,
    legend.position = "top"
  )

if (interactive()) print(g)
ggplot2::ggsave(
  file.path(
    dirname(getwd()),
    "figures/double-target-execution-time-per-target.png"
  ),
  plot = g
)
