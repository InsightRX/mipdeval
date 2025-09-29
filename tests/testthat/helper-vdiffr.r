# All visual tests are skipped by default if vdiffr is not installed, unless
# VDIFFR_RUN_TESTS is explicitly set to "true", which should only be the case on
# a GitHub Actions CI runner with a stable version of R.
if (requireNamespace("vdiffr", quietly = TRUE)) {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # Raise error if vdiffr is not available and visual tests are required:
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    cli::cli_abort("vdiffr is not installed")
  }
  # Otherwise, assign a dummy function:
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
