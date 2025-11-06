

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
  cache$reset()
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
  "sum_exp" = \(t) 1/3 * (exp(- t/2) + exp(-t) + exp(-2*t)),
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
future::plan("multisession")
approximation_output <- furrr::future_pmap(
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  inputs,
  \(target, method, strategy, M, waning_function) {
    im$set_custom_waning(
      custom_function = waning_function,
      target = "infection",
      name = target,
    )

    approx <- im$approximate_compartmental(method = method, M = M, strategy = strategy)

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
generators <- cbind(inputs, tibble::tibble("approx_fun" = plotters))

t <- seq(0, 3, by = 1 / 10)
results <- generators |>
  tidyr::pivot_longer(c("waning_function", "approx_fun"), names_to = "type", values_to = "func") |>
  tidyr::crossing(t) |>
  dplyr::mutate(y = purrr::map2_dbl(.data$func, .data$t, \(f, t) f(t))) |>
  dplyr::select(!c("func"))

appprox_results <- results |>
  dplyr::filter(.data$type == "approx_fun")

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
  ggplot2::facet_grid(M ~ target)



residuals <- dplyr::left_join(
  results,
  reference_results |>
    dplyr::transmute(.data$target, .data$t, "y_ref" = .data$y),
  by = c("t", "target")
) |>
  dplyr::mutate(
    "residual" = .data$y - .data$y_ref,
    "method" = dplyr::if_else(.data$type == "waning_function", NA, .data$method)
  ) |>
  tidyr::pivot_longer(c("y", "residual")) |>
  dplyr::distinct()



residuals |>
  dplyr::filter(.data$target == "sigmoidal_waning", .data$M %in% c(2, 4, 6, 8, 10)) |>
  dplyr::mutate("name" = factor(.data$name, levels = c("y", "residual"))) |>
  ggplot2::ggplot(ggplot2::aes(x = t, y = value, color = method)) +
  ggplot2::geom_line(
    ggplot2::aes(linetype = strategy),
    linewidth = 1
  ) +
  ggplot2::facet_grid(name ~ M, scales = "free")




residuals |>
  dplyr::filter(.data$target == "sigmoidal_waning", .data$M %in% c(2, 4, 6, 8, 10)) |>
  dplyr::mutate("name" = factor(.data$name, levels = c("y", "residual"))) |>
  ggplot2::ggplot(ggplot2::aes(x = t, y = value, color = method)) +
  ggplot2::geom_line(ggplot2::aes(linetype = type), linewidth = 1) +
  ggplot2::facet_grid(M ~ name)






im$set_custom_waning(custom_function = waning_functions[["sum_exp"]])
im$plot(method = "free_delta", M = 4, strategy = "naive")
im$plot(method = "all_free", M = 4, strategy = "naive")
im$plot(method = "all_free", M = 4, strategy = "recursive")



im <- DiseasyImmunity$new()
im$set_exponential_waning()
#im$set_custom_waning(custom_function = waning_functions[["sum_exp"]])
im$plot(M = 3, method = "free_gamma", strategy = "recursive")
