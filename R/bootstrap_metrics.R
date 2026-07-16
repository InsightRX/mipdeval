#' Calculate metrics across bootstrapped folds
#'
#' @description
#' Calculate metrics across bootstrapped folds of the data; optionally, according
#' to one or more grouped variables. Bootstrap size is equal to the size of the
#' data or each grouped variable, and rows are sampled with replacement.
#'
#' @section Useful metrics:
#'
#' - (Normalized) Root mean squared error: [rmse()], [nrmse()]
#' - Mean (absolute) percent error: [mpe()], [mape()]
#' - Accuracy: [accuracy()]
#'
#' @param .data A data frame or data frame extension (e.g. a tibble).
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs of
#'   summary functions to compute across bootstrapped folds of the data. The
#'   name will be the name of the variable in the result.
#' @param .by <[`tidy-select`][dplyr_tidy_select]> Optionally, variables to
#'   group by. Each group is resampled independently.
#' @param .seed Single value for the random seed, used for reproducible random
#'   sampling.
#' @param .n_boots Number of bootstrapped samples to create (per group).
#' @param .conf_level The confidence level to use for the confidence interval.
#'   Must be strictly between 0 and 1. Defaults to a 95 percent confidence
#'   interval.
#'
#' @returns For `bootstrap_metrics()`: A [tibble::tibble()] whose columns are a
#'   combination of the summary expressions and grouping keys that you provide,
#'   plus a `boot` column indicating each bootstrap fold.
#'
#'   For `summarise_bootstrap_metrics()`: A [tibble::tibble()] with, for each
#'   bootstrapped metric, three columns suffixed `_mid`, `_lower`, and `_upper`,
#'   giving the mean and the lower and upper quantiles implied by `.conf_level`,
#'   respectively.
#' @export
#'
#' @examples
#' set.seed(99)
#' df <- data.frame(
#'   observationid = rep(1:1000, 4),
#'   model = rep(c(rep("A", 1000), rep("B", 1000)), 2),
#'   patient_type = "general",
#'   prediction_type = c(rep("a priori", 2000), rep("a posteriori", 2000)),
#'   res = c(
#'     rnorm(1000, 2, 3),
#'     rnorm(1000, 0.1, 1),
#'     rnorm(1000, 1, 3),
#'     rnorm(1000, 0, 0.5)
#'   ),
#'   tdm = rnorm(4000, 10, 5)
#' )
#'
#' boots <- bootstrap_metrics(
#'   df,
#'   rmse = rmse(tdm, tdm - res),
#'   nrmse = nrmse(tdm, tdm - res),
#'   accuracy = accuracy(tdm, tdm - res, 2.5, 0.2),
#'   .by = c(model, patient_type, prediction_type),
#'   .n_boots = 100
#' )
#'
#' summarise_bootstrap_metrics(
#'   boots,
#'   .by = c(model, patient_type, prediction_type)
#' )
bootstrap_metrics <- function(
    .data,
    ...,
    .by = NULL,
    .seed = 10,
    .n_boots = 1000
) {
  withr::local_seed(.seed)
  boot_metric <- seq(.n_boots) |>
    purrr::set_names() |>
    purrr::map(
      \(.x) {
        .data |>
          dplyr::group_by(dplyr::pick({{ .by }})) |>
          dplyr::slice(sample(1:dplyr::n(), dplyr::n(), replace = TRUE)) |>
          dplyr::summarize(..., .groups = "drop")
      }
    ) |>
    purrr::list_rbind(names_to = "boot") |>
    dplyr::mutate(boot = as.numeric(.data$boot))

  boot_metric
}

#' @rdname bootstrap_metrics
#' @export
summarise_bootstrap_metrics <- function(.data, .by = NULL, .conf_level = 0.95) {
  check_conf_level(.conf_level)
  lower <- (1 - .conf_level) / 2
  upper <- 1 - lower
  .data |>
    dplyr::select(-"boot") |>
    dplyr::group_by(dplyr::pick({{ .by }})) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        .fns = list(
          mid = \(.x) mean(.x, na.rm = TRUE),
          lower = \(.x) stats::quantile(.x, lower, na.rm = TRUE, names = FALSE),
          upper = \(.x) stats::quantile(.x, upper, na.rm = TRUE, names = FALSE)
        )
      ),
      .groups = "drop"
    )
}

#' @rdname bootstrap_metrics
#' @export
summarize_bootstrap_metrics <- summarise_bootstrap_metrics

#' Validate a confidence level
#'
#' Internal helper asserting that a confidence level is a single number strictly
#' between 0 and 1.
#'
#' @param conf_level The confidence level to check.
#' @param arg,call Used for error reporting.
#'
#' @returns `conf_level`, invisibly, or throws an error.
#' @keywords internal
check_conf_level <- function(
    conf_level,
    arg = caller_arg(conf_level),
    call = caller_env()
) {
  vctrs::vec_assert(
    conf_level, ptype = numeric(), size = 1L, arg = arg, call = call
  )
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number strictly between 0 and 1, not {conf_level}.",
      call = call
    )
  }
  invisible(conf_level)
}
