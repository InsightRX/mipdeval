#' Check for failed fits / predictions and (optionally) warn
#'
#' Detects predictions that came back as `NA`, which indicates the underlying
#' MAP Bayesian fit failed, and---when `warn = TRUE`---emits a warning
#' summarising how many predictions failed and in which subjects.
#'
#' @param .res output object (`mipdeval_results`) from [run_eval()], or a
#'   `data.frame` with raw results.
#'
#' @returns invisibly, a `data.frame` of the rows with failed predictions.
#' @keywords internal
check_failed_fits <- function(.res) {
  if(inherits(.res, "mipdeval_results")) {
    .res <- .res$results
  }
  errors <- dplyr::filter(
    .res,
    is.na(.data$pred) |
    (is.na(.data$map_ipred) & !.data$apriori) |
    is.na(.data$iter_ipred)
  )
  if(nrow(errors) > 0) {
    cli::cli_warn("Errors were encountered in {nrow(errors)} out of {nrow(.res)} evaluated predictions. The problems occurred in patient(s) {unique(errors$id)}.")
  }
  invisible(errors)
}
