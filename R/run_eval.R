#' Run iterative predictive analysis, looping over each individual's data
#'
#' @inheritParams PKPDmap::get_map_estimates
#'
#' @param data NONMEM-style data.frame, or path to CSV file with NONMEM data
#' @param dictionary data dictionary. Optional, a list that specifies the
#' column names to be used from the dataset.
#' @param groups variable in dataset that groups observations together in
#' iterative flow. By default each observation will be its own "group", but
#' this can be used to group peaks and troughs together, or to group
#' observations on the same day together.
#' @param leak_covariates with the `proseval` tool in PsN, there is “data
#' leakage” (of the future covariates): since the NONMEM dataset in each step
#' contains the covariates for the future, this technically data leakage,
#' and could result in a too optimistic estimate of predictive performance.
#' In `mipdeval`, this is switched off by default (no data leakage), but it
#' can be switched on if we want to match the behavior of `PsN::proseval`
#' exactly.
#' @param incremental should MAP Bayesian do incremental fits in the iterative
#' loop. I.e. in this case it would use the first iterations MAP Bayesian
#' estimates as input for the second iteration, and so forth. The uncertainty
#' around the MAP estimates would be used as the new `omega` matrix. This
#' approach has been called "model predictive control (MPC)"
#' (www.page-meeting.org/?abstract=9076) and may be more predictive than
#' "regular" MAP in some scenarios. Default is `FALSE`.
#' @param threads number of threads to divide computations on. Default is 1,
#' i.e. no parallel execution
#'
#' @export
#'
run_eval <- function(
  model,
  data,
  dictionary = list(id = "ID", evid = "EVID", dv = "DV", mdv = "MDV"),
  parameters = NULL,
  omega = NULL,
  error = NULL,
  groups = NULL,
  weights = NULL,
  prior_weight = 1,
  leak_covariates = FALSE,
  incremental = FALSE,
  threads = 1
) {

  ## 1. read NONMEM data from file or data.frame. Do some simple checks
  input_data <- read_input_data(data) |>
    check_input_data(
      dictionary = dictionary
    )

  ## 2. parse into separate, individual-level datasets, joined into a list object:
  split_data <- split_input_data(
    data = input_data,
    dictionary = dictionary
  )

  ## 3. run the core function on each individual-level dataset in the list
  ##    Use purrr::map() functions and multi-core variant

  ## 4. Combine results into object that has:
  ##    - a data.frame with all raw data
  ##    - a data.frame of all parameter estimates (time-varying). Maybe in same df as 1?
  ##    - a data.frame with computed metrics: RMSE, MPE, MAPE, etc...

  ## 5. Return results

}
