#' Run iterative predictive analysis, looping over each individual's data
#'
#' @inheritParams PKPDmap::get_map_estimates
#'
#' @param model either a PKPDsim model object, or a string pointing to a
#'   PKPDsim-generated model library, e.g. `pkvancothomson`
#' @param data NONMEM-style data.frame, or path to CSV file with NONMEM data
#' @param ids optional, vector of subject IDs to run analysis on (by default
#'   runs analysis on all subjects in dataset)
#' @param dictionary data dictionary. Optional, a named character vector that specifies the column
#'   names to be used from the dataset.
#' @param groups variable in dataset that groups observations together in
#'   iterative flow. By default each observation will be its own "group", but
#'   this can be used to group peaks and troughs together, or to group
#'   observations on the same day together. Grouping will be done prior to
#'   running the analysis, so cannot be changed afterwards.
#' @param censor_covariates with the `proseval` tool in PsN, there is “data
#'   leakage” (of future covariates data): since the NONMEM dataset in each step
#'   contains the covariates for the future, this is technically data leakage,
#'   and could result in an over-optimistic estimate of predictive performance.
#'   In `mipdeval`, covariate censoring of future covariate data is switched on
#'   by default (so no data leakage), but it can be switched off if we want to
#'   match the behavior of `PsN::proseval` exactly.
#' @param incremental should MAP Bayesian do incremental fits in the iterative
#'   loop. I.e. in this case it would use the first iterations MAP Bayesian
#'   estimates as input for the second iteration, and so forth. The uncertainty
#'   around the MAP estimates would be used as the new `omega` matrix. This
#'   approach has been called "model predictive control (MPC)"
#'   (www.page-meeting.org/?abstract=9076) and may be more predictive than
#'   "regular" MAP in some scenarios. Default is `FALSE`.
#' @param threads number of threads to divide computations on. Default is 1,
#'   i.e. no parallel execution
#' @param ruv residual error variability magnitude, specified as list.
#' @param progress should a progress bar be shown? Default is `TRUE`, but when
#'   debugging the package it is useful to have it off, since progress bar
#'   handlers obscure R output.
#'
#' @returns A named list of data frames.
#' @export
run_eval <- function(
  model,
  data,
  ids = NULL,
  parameters = NULL,
  omega = NULL,
  iov_bins = NULL,
  ruv = NULL,
  dictionary = list(),
  groups = NULL,
  weights = NULL,
  weight_prior = 1,
  censor_covariates = TRUE,
  incremental = FALSE,
  threads = 1,
  progress = TRUE
) {

  ## 0. Gather model information in an object
  mod_obj <- parse_model(
    model,
    parameters = parameters,
    ruv = ruv,
    omega = omega,
    iov_bins = iov_bins
  )

  ## 1. read NONMEM data from file or data.frame. Do some simple checks
  input_data <- read_input_data(data) |>
    check_input_data(
      dictionary = dictionary
    )

  ## Select covariates
  covariates <- attr(mod_obj$model, "covariates")

  ## 2. parse into separate, individual-level datasets and parse into
  ##    format convenient for PKPDsim/PKPDmap, joined into a list object:
  data_parsed <- parse_input_data(
    data = input_data,
    covariates = covariates,
    ids = ids
  )

  ## Set up progress bars
  ## Need to loop this through the progressr package
  ## because furrr currently doesn't support progressbars with cli.
  if(progress) {
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
    p <- progressr::progressor(along = data_parsed)
  } else {
    p <- function() { }
  }

  ## 3. run the core function on each individual-level dataset in the list
  if(threads > 1) {
    # TODO: consider using purrr::in_parallel() in the future when it's stable.
    future::plan(future::multisession, workers = threads)
    res <- furrr::future_map(
      .x = data_parsed,
      .f = run_eval_core,
      model = mod_obj$model,
      parameters = mod_obj$parameters,
      omega = mod_obj$omega_matrix,
      ruv = mod_obj$ruv,
      groups = groups,
      censor_covariates = censor_covariates,
      weight_prior = weight_prior,
      progress_function = p
    )
  } else {
    res <- purrr::map(
      .x = data_parsed,
      .f = run_eval_core,
      model = mod_obj$model,
      parameters = mod_obj$parameters,
      omega = mod_obj$omega_matrix,
      ruv = mod_obj$ruv,
      groups = groups,
      censor_covariates = censor_covariates,
      weight_prior = weight_prior,
      progress_function = p
    )
  }

  ## 4. Combine results and basic stats into return object
  res_df <- dplyr::bind_rows(res)
  stats_summ <- res_df |>
    tidyr::pivot_longer(
      cols = c("pred", "map_ipred", "iter_ipred"), names_to = "type"
    ) |>
    dplyr::group_by(.data$type, .data$apriori) |>
    dplyr::summarise(
      rmse = rmse(.data$dv, .data$value),
      mpe = mpe(.data$dv, .data$value),
      mape = mape(.data$dv, .data$value)
    )
  out <- list(
    results = res_df,
    stats = stats_summ
  )

  ## 5. Return results
  out
}
