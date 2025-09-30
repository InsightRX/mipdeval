#' Core iterative simulation and MAP estimation function that loops over
#' an individual's dataset
#'
#' @inheritParams run_eval
#' @param mod_obj list object with model information
#' @param progress_function function to increment progress bar
#'
#' @returns a `data.frame` with individual predictions
#'
run_eval_core <- function(
  mod_obj,
  data,
  weights = NULL,
  weight_prior = 1,
  censor_covariates = TRUE,
  incremental = FALSE,
  progress_function = function() {}
) {

  progress_function()

  obs_data <- data$observations
  comb <- data.frame()
  fit_pars <- data.frame()
  iterations <- unique(obs_data[["_grouper"]])

  for(i in seq_along(iterations)) {

    ## Select which samples should be used in fit, for regular iterative
    ## forecasting and incremental.
    ## TODO: handle weighting down of earlier samples
    weights <- handle_sample_weighting(
      obs_data,
      iterations,
      incremental,
      i
    )

    ## Should covariate data be leaked? PsN::proseval does this,
    ## but for use in MIPD they should be censored.
    cov_data <- handle_covariate_censoring(
      covariates = data$covariates,
      t = obs_data$t[i],
      censor = censor_covariates
    )

    ## Do a fit with PKPDmap, using only the points
    ## with weight=1. The rest of the points will get a predicted value
    ## in the output, but they were not weighted in the fit.
    mod_upd <- mod_obj
    if(incremental & i > 1) {
      mod_upd$parameters <- fit$parameters # take params from previous fit
      mod_upd$omega <- fit$vcov
    }
    fit <- PKPDmap::get_map_estimates(
      model = mod_obj$model,
      parameters = mod_upd$parameters,
      omega = mod_upd$omega,
      error = mod_obj$ruv,
      fixed = mod_obj$fixed,
      as_eta = mod_obj$kappa,
      data = data$observations,
      covariates = cov_data,
      regimen = data$regimen,
      weight_prior = weight_prior,
      weights = weights,
      iov_bins = mod_obj$bins,
      verbose = FALSE
    )

    ## Data frame with predictive data
    pred_data <- tibble::tibble(
      id = obs_data$id,
      t = obs_data$t,
      dv = fit$dv,
      ipred = fit$ipred,
      pred = fit$pred,
      `_iteration` = iterations[i],
      `_grouper` = obs_data$`_grouper`
    )

    ## Add parameter estimates
    fit_pars <- dplyr::mutate(as.data.frame(fit$parameters), id = obs_data$id[1])
    comb <- dplyr::bind_rows(
      comb, dplyr::left_join(pred_data, fit_pars, by = "id")
    )
  }

  ## Get data for MAP fit
  if(incremental) {
    ## need to do an extra MAP Bayesian fit, because we can't use the
    ## incrementally updated parameters + omega
    fit_map <- PKPDmap::get_map_estimates(
      model = mod_obj$model,
      parameters = mod_obj$parameters, # use original model params!
      omega = mod_obj$omega, # use original model params!
      error = mod_obj$ruv,
      fixed = mod_obj$fixed,
      as_eta = mod_obj$kappa,
      data = data$observations,
      covariates = cov_data,
      regimen = data$regimen,
      weight_prior = weight_prior,
      weights = NULL, # no sample weighting, full MAP Bayesian on all samples
      iov_bins = mod_obj$bins,
      verbose = FALSE
    )
    map_pred_data <- tibble::tibble(
      id = obs_data$id,
      t = obs_data$t,
      dv = fit_map$dv,
      ipred = fit_map$ipred,
      pred = fit_map$pred,
      `_iteration` = iterations[i],
      `_grouper` = obs_data$`_grouper`
    )
  } else {  # just take last fit object and pred_data
    fit_map <- fit
    map_pred_data <- pred_data
  }

  ## pre-pend population predictions for the first observation
  # TODO: Refactor this logic into a function or functions, e.g., the first
  # argument to bind_rows() could be refactored into `get_apriori_data()`.
  out <- dplyr::bind_rows(
    comb |>
      dplyr::filter(.data$`_iteration` == 1) |>
      dplyr::mutate(
        `_iteration` = 0,
        ipred = .data$pred
      ) |> # set to population parameters, not individual estimates
        dplyr::select(-!!names(mod_obj$parameters)) |>
        dplyr::left_join(
          dplyr::mutate(as.data.frame(mod_obj$parameters), id = obs_data$id[1]),
          by = "id"
        ),
    comb
  ) |> # and filter only the next grouped prediction for each iteration
    dplyr::filter(.data$`_iteration` == (.data$`_grouper` - 1)) |>
    dplyr::mutate(
      iter_ipred = .data$ipred,
      map_ipred = map_pred_data$ipred, # ipred from full retrospective MAP
      apriori = (.data$`_iteration` == 0)
    ) |>
    dplyr::select(
      "id", "_iteration", "_grouper", "t", "dv", "pred", "map_ipred",
      "iter_ipred", "apriori",
      !!names(mod_obj$parameters)
    )

  out

}

#' Handle covariate censoring
#'
#' This removes new covariate information after a certain time cutoff
#'
#' @param covariates covariates data for single subject
#' @param t timepoint cutoff at which covariate are to be censored
#' (if `censor=FALSE`)
#' @param censor censor covariate data?
#'
#' @returns list with similar shape as `covariates` object, just
#' with potentially censored future data.
#'
handle_covariate_censoring <- function(
  covariates,
  t,
  censor = TRUE
) {
  cov_data <- covariates
  if(censor) {
    for(key in names(cov_data)) {
      idx <- cov_data[[key]]$times <= t
      cov_data[[key]]$times <- cov_data[[key]]$times[idx]
      cov_data[[key]]$value <- cov_data[[key]]$value[idx]
    }
  }
  cov_data
}

#' Handle weighting of samples
#'
#' This function is used to select the samples used in the fit (1 or 0),
#' but also to select their weight, if a sample weighting strategy is
#' selected.
#'
#' @inheritParams run_eval_core
#' @param obs_data tibble or data.frame with observed data for individual
#' @param iterations numeric vector of groups
#' @param i index
#'
#' @returns vector of weights (numeric)
#'
handle_sample_weighting <- function(
  obs_data,
  iterations,
  incremental,
  i
) {
  weights <- rep(0, nrow(obs_data))
  if(incremental) { # just fit current sample or group
    weights[obs_data[["_grouper"]] %in% iterations[i]] <- 1
  } else { # fit all samples up until current sample
    weights[obs_data[["_grouper"]] %in% iterations[1:i]] <- 1
  }
  weights
}
