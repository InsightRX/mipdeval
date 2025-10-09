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
