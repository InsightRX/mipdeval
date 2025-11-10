#' Calculate basic statistics, like RMSE, MPE, MAPE for forecasted data
#'
#' @param res output object (`mipdeval_results`) from `run_eval()`, or
#' `data.frame` with raw results.
#' @param rounding number of decimals to round to.
#' @param acc_error_abs,acc_error_rel For calculating [accuracy()]: Positive number
#'   providing an absolute or relative error margin. The cutoff is exclusive of
#'   the error margin. When `NULL` (the default), accuracy will not be
#'   calculated and will return `NA` instead.
#'
#' @returns tibble
#'
#' @export
calculate_stats <- function(
    res,
    rounding = 3,
    acc_error_abs = NULL,
    acc_error_rel = NULL
) {
  if(inherits(res, "mipdeval_results")) {
    res <- res$results
  }
  out <- res |>
    tidyr::pivot_longer(
      cols = c("pred", "map_ipred", "iter_ipred"), names_to = "type"
    ) |>
    dplyr::group_by(.data$type, .data$apriori) |>
    dplyr::summarise(
      rmse = rmse(.data$dv, .data$value),
      nrmse = nrmse(.data$dv, .data$value),
      mpe = mpe(.data$dv, .data$value),
      mape = mape(.data$dv, .data$value),
      accuracy = dplyr::if_else(
        !is.null(acc_error_abs) & !is.null(acc_error_rel),
        accuracy(.data$dv, .data$value, acc_error_abs, acc_error_rel),
        NA
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), round, rounding)) |>
    dplyr::as_tibble()
  class(out) <- c("mipdeval_results_stats_summ", class(out))
  out
}

#' Options for summary statistics
#'
#' @param ... These dots are reserved for future extensibility and must be empty.
#' @inheritParams calculate_stats
#'
#' @returns A list.
#' @export
stats_summ_options <- function(
    ...,
    rounding = 3,
    acc_error_abs = NULL,
    acc_error_rel = NULL
) {
  rlang::check_dots_empty()
  check_required_accuracy(acc_error_abs, acc_error_rel)
  out <- list(
    rounding = vctrs::vec_assert(
      rounding, ptype = numeric(), size = 1L, arg = "rounding"
    ),
    acc_error_abs = vec_assert_or_null(
      acc_error_abs, ptype = numeric(), size = 1L, arg = "acc_error_abs"
    ),
    acc_error_rel = vec_assert_or_null(
      acc_error_rel, ptype = numeric(), size = 1L, arg = "acc_error_rel"
    )
  )
  structure(out, class = "mipdeval_stats_summ_options")
}

check_required_accuracy <- function(
  acc_error_abs = NULL,
  acc_error_rel = NULL,
  call = caller_env()
) {
  acc_error_abs_null <- is.null(acc_error_abs) & !is.null(acc_error_rel)
  acc_error_rel_null <- !is.null(acc_error_abs) & is.null(acc_error_rel)
  if (!acc_error_abs_null & !acc_error_rel_null) return()
  cli::cli_abort(
    message = c(
      paste0(
        "{.arg acc_error_abs} and {.arg acc_error_rel} must both be specified ",
        "to calculate accuracy."
      ),
      "x" = dplyr::case_when(
        acc_error_abs_null ~ "{.arg acc_error_abs} is NULL.",
        acc_error_rel_null ~ "{.arg acc_error_rel} is NULL."
      )
    ),
    call = call
  )
}
