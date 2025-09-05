#' Get required covariates from a PKPDsim model object
#'
#' @param model PKPDsim model object
#'
get_required_covariates <- function(model) {
  unique(purrr::list_c(purrr::map(model, "covariates")))
}

#' For requested columns in a dataset, check if values vary or not across
#' rows
#'
#' @param .data data.frame
#' @param .cols vector of column names
#'
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
rmse <- function (obs, pred) {
  res_sq <- (pred - obs)^2
  sqrt(mean(res_sq, na.rm = T))
}

#' Mean absolute prediction error
#'
#' @inheritParams rmse
#'
mape <- function (obs, pred) {
  sum(abs((obs - pred))/obs)/length(obs)
}

#' Mean prediction error
#'
#' @inheritParams rmse
#'
mpe <- function (obs, pred) {
  sum((obs - pred)/obs)/length(obs)
}


