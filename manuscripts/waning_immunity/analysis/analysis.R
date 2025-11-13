# TODOs
# Should we set individual_level = FALSE and / or monotonous = FALSE?

# Controls for the outputs

# Set figure targets
single_target <- "sigmoidal_waning"
M_subset <- c(2, 6, 10)

# Set optimiser parameters
monotonous <- FALSE
individual_level <- FALSE






# Lockfile should only be created when getting a new verison of diseasy
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
.libPaths(file.path(getwd(), "r-library"))
stopifnot(endsWith(.libPaths()[1], "r-library"))



library(diseasy)


## First figure
# For our first figure, we want to show the performance of the different methods in approximating some simple target curves
# as the number of compartments increase.

# For each of the three methods, we want to show their approximation for M = 1, 3, and 5 against a sigmoidal target
# and a sum of exponentials.


im <- DiseasyImmunity$new()

# Define custom waning functions
waning_functions <- list(
  "sigmoidal_waning" = \(t) exp(-(t - 1) * 6) / (1 + exp(-(t - 1) * 6)),
  "sum_exp" = \(t) 1 / 3 * (exp(- t/2) + exp(-t) + exp(-2 * t)),
  "exponential" = \(t) exp(-t)
)

# Get combinations of problems to solve
inputs <- tidyr::expand_grid(
  "target" = names(waning_functions),
  "method-strategy" = c(
    "free_gamma-naive", "free_gamma-recursive",
    "free_delta-naive", "free_delta-recursive",
    "all_free-naive", "all_free-recursive", "all_free-combination"
  ),
  #"M" = c(1, 2, 3, 4)
  "M" = seq.int(from = 1, to = 10, by = 1)
) |>
  tidyr::separate_wider_delim(
    cols = "method-strategy",
    delim = "-",
    names = c("method", "strategy")
  ) |>
  dplyr::left_join(
    tibble::enframe(waning_functions, name = "target", value = "waning_function"),
    by = "target"
  )

# Generate approximations (stored in the cache)
future::plan("multisession", gc = TRUE, workers = unname(future::availableCores(omit = 1)))
approximation_output <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs,
  \(target, method, strategy, M, waning_function) {
    im$set_custom_waning(
      custom_function = waning_functions[["exponential"]],
      target = "infection",
      name = "exponential",
    )

    approx <- im$approximate_compartmental(
      method = "all_free",
      M = 4,
      strategy = "combination",
      monotonous = monotonous,
      individual_level = individual_level
    )

    list(
      "gamma" = approx$gamma$infection,
      "delta" = approx$delta,
      "method" = method,
      "strategy" = strategy,
      "M" = M,
      "target" = target
    )
  }
)

# Get a reference to the internal helper functions
private <- im$.__enclos_env__$private


# Convert gamma and delta values to plotting functions
plotters <- purrr::map(
  approximation_output,
  ~ private$get_approximation(
    purrr::pluck(.x, "gamma"),
    purrr::pluck(.x, "delta"),
    purrr::pluck(.x, "M")
  )
)

# Generate data.frame for plotting
generators <- cbind(inputs, tibble::tibble("approx_function" = plotters)) |>
  tibble::as_tibble()


t <- seq(0, 3.5, by = 1 / 10)
results <- generators |>
  tidyr::pivot_longer(c("waning_function", "approx_function"), names_to = "type", values_to = "func") |>
  tidyr::crossing(t) |>
  dplyr::mutate(y = purrr::map2_dbl(.data$func, .data$t, \(f, t) f(t))) |>
  dplyr::select(!c("func"))

appprox_results <- results |>
  dplyr::filter(.data$type == "approx_function")

reference_results <- results |>
  dplyr::filter(.data$type == "waning_function") |>
  dplyr::select(!c("M", "method", "strategy")) |>
  dplyr::distinct()

ggplot2::ggplot(mapping = ggplot2::aes(x = t, y = y, color = method)) +
  ggplot2::geom_line(
    data = appprox_results,
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
error <- generators |>
  #dplyr::filter(.data$target == !!single_target) |>
  dplyr::mutate(
    "error" = purrr::map2_dbl(
      .x = .data$waning_function,
      .y = .data$approx_function,
      ~ stats::integrate(
        \(t) (.x(t) - .y(t))^2,
        lower = 0,
        upper = Inf
      )$value
    )
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
      #.data$target == !!single_target,
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
      glue::glue("Total error from approximation to single target ({single_target})"),
      glue::glue("individual_level = {individual_level}, monotonous = {monotonous}")
    )
  )

ggplot2::ggsave("../figures/2.png")
