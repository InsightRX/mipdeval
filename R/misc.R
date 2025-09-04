#' Get required covariates from a PKPDsim model object
#'
get_required_covariates <- function(model_object) {
  unique(purrr::list_c(purrr::map(model_object, "covariates")))
}

#' For requested columns in a dataset, check if values vary or not across
#' rows
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

#' Stats functions
rmse <- function (obs, pred) {
  res_sq <- (pred - obs)^2
  sqrt(mean(res_sq, na.rm = T))
}

mape <- function (obs, pred) {
  sum(abs((obs - pred))/obs)/length(obs)
}

mpe <- function (obs, pred) {
  sum((obs - pred)/obs)/length(obs)
}


