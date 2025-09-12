#' Core iterative simulation and MAP estimation function that loops over
#' an individual's dataset
#'
#' @inheritParams run_eval
#' @param progress_function function to increment progress bar
#'
#' @returns a `data.frame` with individual predictions
#'
#' @export
#'
run_eval_core <- function(
  mod_obj,
  data,
  parameters = NULL,
  omega = NULL,
  ruv = NULL,
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

    ## TODO: this will change once we support sample-weighting strategies
    ##       For now this is just simple full-weighting for all previous points
    weights <- rep(0, nrow(obs_data))
    weights[obs_data[["_grouper"]] %in% iterations[1:i]] <- 1

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
    fit <- PKPDmap::get_map_estimates(
      model = mod_obj$model,
      parameters = mod_obj$parameters,
      omega = mod_obj$omega_matrix,
      error = mod_obj$ruv,
      fixed = mod_obj$fixed,
      data = data$observations,
      covariates = cov_data,
      regimen = data$regimen,
      weight_prior = weight_prior,
      weights = weights
    )

    ## Data frame with predictive data
    pred_data <- data.frame(
      id = obs_data$id,
      t = obs_data$t,
      dv = fit$dv,
      ipred = fit$ipred,
      pred = fit$pred
    ) |>
      dplyr::mutate(
        `_iteration` = iterations[i],
        `_grouper` = obs_data$`_grouper`
      )

    ## Add parameter estimates
    fit_pars <- as.data.frame(fit$parameters) |>
      dplyr::mutate(id = obs_data$id[1])
    comb <- dplyr::bind_rows(
      comb,
      pred_data |>
        dplyr::left_join(fit_pars, by = "id")
    )
  }

  ## pre-pend population predictions for the first observation
  out <- dplyr::bind_rows(
    comb |>
      dplyr::filter(`_iteration` == 1) |>
      dplyr::mutate(
        `_iteration` = 0,
        ipred = pred
      ) |> # set to population parameters, not individual estimates
        dplyr::select(-!!names(parameters)) |>
        dplyr::left_join(
          as.data.frame(parameters) |>
            dplyr::mutate(id = obs_data$id[1]),
          by = "id"
        ),
    comb
  ) |> # and filter only the next grouped prediction for each iteration
    dplyr::filter(`_iteration` == (`_grouper` - 1)) |>
    dplyr::mutate(
      iter_ipred = ipred,
      map_ipred = pred_data$ipred, # ipred from last fit (full MAP)
      apriori = (`_iteration` == 0)
    ) |>
    dplyr::select(
      id, `_iteration`, `_grouper`, t, dv, pred, map_ipred, iter_ipred, apriori,
      !!names(parameters)
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
