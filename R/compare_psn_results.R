#' Compare mipdeval results with PsN
#'
#' @description
#' The `compare_psn_*_results()` and `reldiff_psn_*_results()` family of
#' functions are designed to compare mipdeval results with PsN:
#'
#' - `compare_psn_proseval_results()` and `compare_psn_execute_results()` summarize
#'   the relative difference in predictions between mipdeval and the PsN `execute`
#'   and `proseval` tools.
#' - `reldiff_psn_proseval_results()` and `reldiff_psn_execute_results` calculate
#'   the relative difference for each prediction between mipdeval and the PsN
#'   `execute` and `proseval` tools.
#'
#' @param mipdeval The `run_eval()` object.
#' @param psn A PsN execute or proseval data set.
#' @param tol Optional. Tolerance.
#'
#' @returns A data/frame.
#' @name compare-psn-results
NULL

#' @rdname compare-psn-results
#' @export
compare_psn_proseval_results <- function(mipdeval, psn, tol = 0.1) {
  compare_psn_results(mipdeval, psn, tol, .apriori = FALSE)
}

#' @rdname compare-psn-results
#' @export
reldiff_psn_proseval_results <- function(mipdeval, psn) {
  reldiff_psn_results(mipdeval, psn, .apriori = FALSE)
}

#' @rdname compare-psn-results
#' @export
compare_psn_execute_results <- function(mipdeval, psn, tol = 0.1) {
  compare_psn_results(mipdeval, psn, tol, .apriori = TRUE)
}

#' @rdname compare-psn-results
#' @export
reldiff_psn_execute_results <- function(mipdeval, psn) {
  reldiff_psn_results(mipdeval, psn, .apriori = TRUE)
}

compare_psn_results <- function(mipdeval, psn, tol = 0.1, .apriori) {
  # TODO: Should return summary statistics like mean abs rel diff, min and max
  # rel diff, whether all observations were within tolerance
  dplyr::summarise(
    reldiff_psn_results(mipdeval, psn, .apriori),
    within_tol = all(abs(.data$rel_diff) < tol)
  )
}

reldiff_psn_results <- function(mipdeval, psn, .apriori) {
  # TODO: Generalize to work with `execute` and `proseval`. Currently works for
  # proseval.
  mipdeval$results |>
    dplyr::filter(.data$apriori == .apriori) |>
    dplyr::mutate(psn_pred = psn$pred) |>
    dplyr::mutate(
      df = .data$iter_ipred - .data$psn_pred,
      rel_diff = .data$df / .data$psn_pred
    ) |>
    dplyr::arrange(.data$rel_diff)
}
