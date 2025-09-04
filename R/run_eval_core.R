#' Core iterative simulation and MAP estimation function that loops over
#' an individual's dataset
#'
#' @inheritParams run_eval
#'
#' @returns a `data.frame` with individual predictions
#'
#' @export
#'
run_eval_core <- function(
  model,
  data,
  parameters = NULL,
  omega = NULL,
  ruv = NULL,
  groups = NULL,
  weights = NULL,
  prior_weight = 1,
  leak_covariates = FALSE,
  incremental = FALSE
) {

  obs_data <- data$observations
  comb <- data.frame()
  for(i in 1:nrow(obs_data)) {
    weights <- rep(0, nrow(obs_data))
    weights[1:i] <- 1
    fit <- PKPDmap::get_map_estimates(
      model = model,
      parameters = parameters,
      omega = omega,
      error = ruv,
      fixed = c(attr(model, "fixed"), "TDM_INIT"),
      data = data$observations,
      covariates = data$covariates,
      regimen = data$regimen,
      weights = weights
    )
    pred_data <- data.frame(
      id = obs_data$id,
      t = obs_data$t,
      dv = fit$dv,
      ipred = fit$ipred,
      pred = fit$pred,
      iter = i,
      group = 1:length(fit$dv) # simple grouping by sample for now, change!
    )
    comb <- dplyr::bind_rows(
      comb,
      pred_data
    )
  }

  ## pre-pend population predictions for the first observation
  out <- dplyr::bind_rows(
    comb |>
      dplyr::filter(iter == 1) |>
      dplyr::mutate(
        iter = 0,
        ipred = pred
      ),
    comb
  ) |> # and filter only the next grouped prediction for each iteration
    dplyr::filter(iter == (group - 1)) |>
    dplyr::mutate(
      iter_ipred = ipred,
      map_ipred = pred_data$ipred, # ipred from last fit (full MAP)
      apriori = (iter == 0)
    ) |>
    dplyr::select(id, t, dv, pred, map_ipred, iter_ipred, iter, group, apriori)

  out

}
