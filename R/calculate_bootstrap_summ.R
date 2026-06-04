#' Bootstrap a confidence interval for forecasting error metrics
#'
#' Bootstraps point and confidence interval estimates of forecasting error
#' metrics from the results of [run_eval()]. This is the engine behind the
#' `bootstrap_summ` element of a `run_eval()` result; it is exported so that the
#' bootstrap summary can be (re)computed, for example with additional grouping
#' variables or a different set of metrics, without re-running the analysis.
#'
#' For each row, the prediction that is evaluated depends on whether the
#' prediction is *a priori* or *a posteriori*: a priori predictions are evaluated
#' against the population prediction (`pred`) and a posteriori predictions
#' against the iterative individual prediction (`iter_ipred`). Metrics are always
#' grouped by `apriori`; additional grouping columns can be supplied via `.by`.
#'
#' @param .res output object (`mipdeval_results`) from [run_eval()], or a
#'   `data.frame` with raw results (the `results` element).
#' @param error_metrics Character vector of error metrics to bootstrap. One or
#'   more of `"rmse"`, `"nrmse"`, `"mpe"`, `"mape"`, and `"accuracy"`.
#' @param acc_error_abs,acc_error_rel For the `"accuracy"` metric: Positive
#'   number providing an absolute or relative error margin. See [accuracy()].
#'   Both must be supplied when `"accuracy"` is requested.
#' @param n_boots Number of bootstrapped samples to create (per group).
#' @param seed Single value for the random seed, used for reproducible random
#'   sampling.
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly between 0 and 1. Defaults to a 95 percent confidence
#'   interval.
#' @param .by Optional character vector of additional columns to group by, on top
#'   of `apriori`.
#'
#' @returns A [tibble::tibble()] with one row per `apriori` (and `.by`) group and,
#'   for each requested metric, columns suffixed `_mid`, `_lower`, and `_upper`.
#' @export
calculate_bootstrap_summ <- function(
  .res,
  error_metrics = c("rmse", "mpe"),
  acc_error_abs = NULL,
  acc_error_rel = NULL,
  n_boots = 1000,
  seed = 123,
  conf_level = 0.95,
  .by = NULL
) {
  if (inherits(.res, "mipdeval_results")) {
    .res <- .res$results
  }

  exprs <- bootstrap_metric_exprs(error_metrics, acc_error_abs, acc_error_rel)

  ## Evaluate a priori predictions against the population prediction and
  ## a posteriori predictions against the iterative individual prediction.
  .res <- dplyr::mutate(
    .res,
    .pred_eval = dplyr::if_else(.data$apriori, .data$pred, .data$iter_ipred)
  )

  by_cols <- c("apriori", .by)

  boots <- bootstrap_metrics(
    .res,
    !!!exprs,
    .by = !!by_cols,
    .seed = seed,
    .n_boots = n_boots
  )

  out <- summarise_bootstrap_metrics(
    boots,
    .by = !!by_cols,
    .conf_level = conf_level
  )
  class(out) <- c("mipdeval_results_bootstrap_summ", class(out))
  out
}

#' Build a named list of metric expressions for bootstrapping
#'
#' Internal helper mapping the `error_metrics` character vector accepted by
#' [calculate_bootstrap_summ()] and [bootstrap_options()] to data-masking
#' expressions evaluated over the columns `dv` (observed) and `.pred_eval`
#' (the prediction selected per row).
#'
#' @inheritParams calculate_bootstrap_summ
#' @param call The calling environment, for error reporting.
#'
#' @returns A named list of quosures, in the order requested.
#' @noRd
bootstrap_metric_exprs <- function(
    error_metrics,
    acc_error_abs = NULL,
    acc_error_rel = NULL,
    call = rlang::caller_env()
) {
  lookup <- list(
    rmse  = rlang::quo(rmse(.data$dv, .data[[".pred_eval"]])),
    nrmse = rlang::quo(nrmse(.data$dv, .data[[".pred_eval"]])),
    mpe   = rlang::quo(mpe(.data$dv, .data[[".pred_eval"]])),
    mape  = rlang::quo(mape(.data$dv, .data[[".pred_eval"]])),
    accuracy = rlang::quo(
      accuracy(.data$dv, .data[[".pred_eval"]], !!acc_error_abs, !!acc_error_rel)
    )
  )
  error_metrics <- rlang::arg_match(
    error_metrics, values = names(lookup), multiple = TRUE, error_call = call
  )
  if ("accuracy" %in% error_metrics) {
    # check_required_accuracy() handles the "exactly one supplied" case, but
    # returns silently when both are NULL (its "do not compute" signal); when
    # accuracy is explicitly requested, both margins are required.
    check_required_accuracy(acc_error_abs, acc_error_rel, call = call)
    if (is.null(acc_error_abs) && is.null(acc_error_rel)) {
      cli::cli_abort(
        paste0(
          "{.arg acc_error_abs} and {.arg acc_error_rel} must both be ",
          "specified to calculate accuracy."
        ),
        call = call
      )
    }
  }
  lookup[error_metrics]
}

#' Options for bootstrapping error metrics
#'
#' @param ... These dots are reserved for future extensibility and must be empty.
#' @param error_metrics Character vector of error metrics to bootstrap. One or
#'   more of `"rmse"`, `"nrmse"`, `"mpe"`, `"mape"`, and `"accuracy"`. `"accuracy"`
#'   is not bootstrapped by default; when requested, its error margins are taken
#'   from [stats_summ_options()] (`acc_error_abs` and `acc_error_rel`), both of
#'   which must be set.
#' @param n_boots Number of bootstrapped samples to create (per group).
#' @param seed Single value for the random seed, used for reproducible random
#'   sampling.
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly between 0 and 1. Defaults to a 95 percent confidence
#'   interval.
#' @param skip Logical. Skip bootstrap simulations? Defaults to `TRUE`, since
#'   bootstrapping can be expensive.
#'
#' @returns A list.
#' @export
bootstrap_options <- function(
    ...,
    error_metrics = c("rmse", "nrmse", "mpe", "mape"),
    n_boots = 1000,
    seed = 123,
    conf_level = 0.95,
    skip = TRUE
) {
  rlang::check_dots_empty()
  error_metrics <- rlang::arg_match(
    error_metrics,
    values = c("rmse", "nrmse", "mpe", "mape", "accuracy"),
    multiple = TRUE
  )
  check_conf_level(conf_level)
  out <- list(
    error_metrics = error_metrics,
    n_boots = vctrs::vec_assert(
      n_boots, ptype = numeric(), size = 1L, arg = "n_boots"
    ),
    seed = seed,
    conf_level = conf_level,
    skip = vctrs::vec_assert(
      skip, ptype = logical(), size = 1L, arg = "skip"
    )
  )
  structure(out, class = "mipdeval_bootstrap_options")
}
