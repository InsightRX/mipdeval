#' Get required covariates from a PKPDsim model object
#'
#' @param model PKPDsim model object
#'
#' @returns A character vector of covariate names.
get_required_covariates <- function(model) {
  unique(purrr::list_c(purrr::map(model, "covariates")))
}

#' For requested columns in a dataset, check if values vary or not across
#' rows
#'
#' @param .data data.frame
#' @param .cols vector of column names
#'
#' @returns A data.frame
is_timevarying <- function(.data, .cols) {
  out <- dplyr::summarise(
    .data, dplyr::across(dplyr::all_of(.cols), \(.x) length(unique(.x)) > 1)
  )
  tidyr::pivot_longer(
    out,
    cols = dplyr::everything(),
    names_to = "covariate",
    values_to = "timevarying"
  )
}

#' Root-mean-squared error
#'
#' @param obs observations vector
#' @param pred predictions vector
#'
#' @returns A numeric vector
rmse <- function (obs, pred) {
  res_sq <- (pred - obs)^2
  sqrt(mean(res_sq, na.rm = TRUE))
}

#' Normalized root-mean-squared error
#'
#' @param obs observations vector
#' @param pred predictions vector
#'
#' @returns A numeric vector
#'
nrmse <- function (obs, pred) {
  res_sq <- (pred - obs)^2
  rmse <- sqrt(mean(res_sq, na.rm = T))
  rmse/mean(obs, na.rm = T)
}

#' Mean absolute percentage error
#'
#' @inheritParams rmse
#'
#' @returns A numeric vector
mape <- function (obs, pred) {
  sum(abs((obs - pred))/obs)/length(obs)
}

#' Mean percentage error
#'
#' @inheritParams rmse
#'
#' @returns A numeric vector
mpe <- function (obs, pred) {
  sum((obs - pred)/obs)/length(obs)
}

#' Accuracy
#'
#' Accuracy provides a measure of clinical suitability, defined by whether model
#' predicted drug concentrations fall within an absolute OR relative error
#' margin of the measured concentrations.
#'
#' @param obs Observations vector.
#' @param pred Predictions vector.
#' @param error_abs,error_rel Positive number providing an absolute or relative
#'   error margin. The cutoff is exclusive of the error margin. Defaults to `0`,
#'   meaning no predictions fall within the error margin.
#'
#' @returns For `is_accurate()`, `is_accurate_abs()`, and `is_accurate_rel()`: A
#'   logical vector indicating whether or not each predicted drug concentration
#'   was considered accurate according to the specified absolute or relative
#'   error margin(s).
#'
#'   For `accuracy()`: A single value between 0 and 1 indicating the proportion
#'   of predicted drug concentrations that fell within the specified absolute
#'   and relative error margins.
#'
#' @examples
#' # Does the predicted drug concentration fall within 0.5 mg/L error margin?
#' is_accurate_abs(6, 5, error_abs = 0.5)
#'
#' # Does the predicted drug concentration fall within 25% error margin?
#' is_accurate_rel(6, 5, error_rel = 0.25)
#'
#' # Does the predicted drug concentration fall within 0.5 mg/L OR 25%?
#' is_accurate(6, 5, error_abs = 0.5, error_rel = 0.25)
#'
#' # What proportion of predicted drug concentrations fell within 0.5 mg/L OR 25%?
#' accuracy(rnorm(10, 6), rnorm(10, 5), error_abs = 0.5, error_rel = 0.25)
#'
#' @export
accuracy <- function(obs, pred, error_abs = 0, error_rel = 0) {
  mean(is_accurate(obs, pred, error_abs, error_rel))
}

#' @rdname accuracy
#' @export
is_accurate <- function(obs, pred, error_abs = 0, error_rel = 0) {
  is_accurate_abs(obs, pred, error_abs) | is_accurate_rel(obs, pred, error_rel)
}

#' @rdname accuracy
#' @export
is_accurate_abs <- function(obs, pred, error_abs = 0) {
  abs(pred - obs) < error_abs
}

#' @rdname accuracy
#' @export
is_accurate_rel <- function(obs, pred, error_rel = 0) {
  (pred/obs > 1 - error_rel) & (pred/obs < 1 + error_rel)
}

#' Weighted sum-of-squares of residuals
#'
#' @inheritParams rmse
#' @param w weights
#'
ss <- function(obs, pred, w = NULL) {
  if(is.null(w)) {
    w <- rep(1, length(obs))
  }
  if (length(obs) != length(pred) || length(obs) != length(w)) {
    cli::cli_abort("`obs`, `pred`, and `w` must have the same length")
  }
  if(sum(w) == 0) return(NA)
  sum(w * (obs - pred)^2)
}

