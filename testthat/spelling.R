if (rlang::is_installed("spelling") && !Sys.getenv("CI", unset = FALSE)) {
  spelling::spell_check_test(
    vignettes = TRUE,
    error = FALSE,
    skip_on_cran = TRUE
  )
}
