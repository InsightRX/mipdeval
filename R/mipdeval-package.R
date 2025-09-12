#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom utils read.csv
## usethis namespace: end
NULL

# Silence NOTE about PKPDsim model library functions: https://dplyr.tidyverse.org/articles/in-packages.html#join-helpers
utils::globalVariables(c(
  "model", "parameters", "ruv", "omega_matrix", "iov", "fixed"
))
