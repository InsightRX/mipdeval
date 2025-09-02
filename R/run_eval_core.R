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
  error = NULL,
  groups = NULL,
  weights = NULL,
  prior_weight = 1,
  leak_covariates = FALSE,
  incremental = FALSE
) {

  ## 1. loop over groups in data (usually just the concentrations)
  ##
  ##    a. set the appropriate weights flags so that data 1:n is fitted
  ##    b. run get_map_estimates() on the data, make sure all observations are predicted out
  ##    /end loop
  ##
  ## 2. format data and return data.frame

}
