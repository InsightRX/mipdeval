#' Bootstrap confidence intervals for forecasting error metrics
#'
#' Bootstraps point and confidence interval estimates of forecasting error
#' metrics from the results of [run_eval()]. This is the engine behind the
#' `bootstrap_summ` element of a `run_eval()` result; it is exported so that the
#' bootstrap summary can be (re)computed, for example with additional grouping
#' variables, without re-running the analysis.
#'
#' @param .res output object (`mipdeval_results`) from [run_eval()], or a
#'   `data.frame` with raw results (the `results` element).
#' @param acc_error_abs,acc_error_rel Positive number providing an absolute or
#'   relative error margin for the accuracy metric. See [accuracy()]. Accuracy is
#'   bootstrapped only when both are supplied.
#' @param n_boots Number of bootstrapped samples to create (per group).
#' @param seed Single value for the random seed, used for reproducible random
#'   sampling.
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly between 0 and 1. Defaults to a 95 percent confidence
#'   interval.
#' @param .by Optional character vector of additional columns to group by, on top
#'   of `apriori`.
#'
#' @details
#' The following error metrics are bootstrapped: RMSE, NRMSE, MPE, and MAPE,
#' plus accuracy when both error margins are supplied.
#'
#' For each row, the prediction that is evaluated depends on whether the
#' prediction is *a priori* or *a posteriori*: a priori predictions are evaluated
#' against the population prediction (`pred`) and a posteriori predictions
#' against the iterative individual prediction (`iter_ipred`). Metrics are always
#' grouped by `apriori`; additional grouping columns can be supplied via `.by`.
#'
#' @returns A [tibble::tibble()] with one row per `apriori` (and `.by`) group and,
#'   for each metric, columns suffixed `_mid`, `_lower`, and `_upper`.
#' @export
calculate_bootstrap_summ <- function(
  .res,
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

  exprs <- bootstrap_metric_exprs(acc_error_abs, acc_error_rel)

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
#' Internal helper returning data-masking expressions for each forecasting error
#' metric, evaluated over the columns `dv` (observed) and `.pred_eval` (the
#' prediction selected per row). Accuracy is included only when both error
#' margins are supplied, mirroring [calculate_stats()].
#'
#' @inheritParams calculate_bootstrap_summ
#' @param call The calling environment, for error reporting.
#'
#' @returns A named list of quosures.
#' @noRd
bootstrap_metric_exprs <- function(
    acc_error_abs = NULL,
    acc_error_rel = NULL,
    call = rlang::caller_env()
) {
  exprs <- list(
    rmse  = rlang::quo(rmse(.data$dv, .data[[".pred_eval"]])),
    nrmse = rlang::quo(nrmse(.data$dv, .data[[".pred_eval"]])),
    mpe   = rlang::quo(mpe(.data$dv, .data[[".pred_eval"]])),
    mape  = rlang::quo(mape(.data$dv, .data[[".pred_eval"]]))
  )
  # Accuracy can only be computed when error margins are supplied. When neither
  # is supplied it is simply omitted; check_required_accuracy() errors when
  # exactly one is supplied.
  if (!is.null(acc_error_abs) || !is.null(acc_error_rel)) {
    check_required_accuracy(acc_error_abs, acc_error_rel, call = call)
    exprs$accuracy <- rlang::quo(
      accuracy(.data$dv, .data[[".pred_eval"]], !!acc_error_abs, !!acc_error_rel)
    )
  }
  exprs
}

#' Options for bootstrapping error metrics
#'
#' Configures bootstrapping of confidence intervals around the forecasting error
#' metrics. The result is passed to the `bootstrap` argument of
#' [stats_summ_options()] (which also supplies the accuracy error margins shared
#' with the point estimates). All metrics are bootstrapped; there is no metric
#' selection here.
#'
#' @param ... These dots are reserved for future extensibility and must be empty.
#' @param n_boots Number of bootstrapped samples to create (per group).
#' @param seed Single value for the random seed, used for reproducible random
#'   sampling.
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly between 0 and 1. Defaults to a 95 percent confidence
#'   interval.
#' @param skip Logical. Skip bootstrapping? Defaults to `TRUE`, since
#'   bootstrapping can be expensive.
#'
#' @returns A list.
#' @export
bootstrap_options <- function(
    ...,
    n_boots = 1000,
    seed = 123,
    conf_level = 0.95,
    skip = TRUE
) {
  rlang::check_dots_empty()
  check_conf_level(conf_level)
  out <- list(
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
