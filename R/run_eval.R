#' Run iterative predictive analysis, looping over each individual's data
#'
#' @inheritParams PKPDmap::get_map_estimates
#'
#' @param model either a PKPDsim model object, or a string pointing to a
#'   PKPDsim-generated model library, e.g. `pkvancothomson`
#' @param data NONMEM-style data.frame, or path to CSV file with NONMEM data
#' @param ids optional, vector of subject IDs to run analysis on (by default
#'   runs analysis on all subjects in dataset)
#' @param dictionary data dictionary. Optional, a named character vector that
#'   specifies the column names to be used from the dataset.
#' @param group name of column in `data` that groups observations together in
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
#' @param .vpc_options Options for VPC simulations. This must be the result from
#'   a call to [vpc_options()].
#' @param threads number of threads to divide computations on. Default is 1,
#'   i.e. no parallel execution
#' @param ruv residual error variability magnitude, specified as list.
#' @param iov a list specifying the required metadata for implementation of IOV,
#'   specifically the coefficient of variation (CV %) of the IOV for each
#'   parameter and a vector of bin separators. For example,
#'   `list(cv = list(CL = 0.1, V = 0.2), bins = c(0, 24, 48, 9999))`
#' @param progress should a progress bar be shown? Default is `TRUE`, but when
#'   debugging the package it is useful to have it off, since progress bar
#'   handlers obscure R output.
#'
#' @returns A named list of data frames.
#'
#' @export
run_eval <- function(
  model,
  data,
  ids = NULL,
  parameters = NULL,
  fixed = NULL,
  omega = NULL,
  iov = NULL,
  ruv = NULL,
  dictionary = list(),
  group = NULL,
  weights = NULL,
  weight_prior = 1,
  censor_covariates = TRUE,
  incremental = FALSE,
  .vpc_options = vpc_options(),
  threads = 1,
  progress = TRUE,
  verbose = TRUE
) {

  if(progress) { # configure progressbars
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
  }

  ## 0. Gather model information in an object
  mod_obj <- parse_model(
    model,
    parameters = parameters,
    ruv = ruv,
    omega = omega,
    fixed = fixed,
    iov = iov
  )

  ## 1. read NONMEM data from file or data.frame. Do some simple checks
  if(verbose) cli::cli_alert_info("Reading and parsing input data")
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
    ids = ids,
    group = group
  )

  ## Set up progress bars
  ## Need to loop this through the progressr package
  ## because furrr currently doesn't support progressbars with cli.
  p <- if(progress) { progressr::progressor(along = data_parsed) } else { \(x) {} }

  ## 3. run the core function on each individual-level dataset in the list
  if(verbose) {
    cli::cli_alert_info("Running forecasts for subjects in dataset")
    cli::cli_progress_step(
      msg = NULL,
      msg_done = "Forecasting analysis done."
    )
  }
  res <- run(
    .x = data_parsed,
    .f = run_eval_core,
    mod_obj = mod_obj,
    censor_covariates = censor_covariates,
    weight_prior = weight_prior,
    progress_function = p,
    .threads = threads,
    .skip = .vpc_options$vpc_only
  )
  cli::cli_progress_done()

  if(verbose) {
    if(.vpc_options$skip) {
      cli::cli_alert_info("Skipping simulations for VPC / NPDE")
    } else {
      cli::cli_alert_info("Running simulations for VPC / NPDE")
      cli::cli_progress_step(
        msg = NULL,
        msg_done = "Simulations for VPC / NPDE done"
      )
    }
  }
  p <- if(progress) { progressr::progressor(along = data_parsed) } else { \(x) {} }
  res_vpc <- run(
    .x = data_parsed,
    .f = run_vpc_core,
    mod_obj = mod_obj,
    n_samples = .vpc_options$n_samples,
    seed = .vpc_options$seed,
    progress_function = p,
    .threads = threads,
    .skip = .vpc_options$skip
  )
  cli::cli_progress_done()
  # NULL is returned if VPC is skipped.
  if (!is.null(res_vpc)) {
    res_vpc <- dplyr::arrange(res_vpc, .data$ITER, .data$ID, .data$TIME)
  }

  ## 4. Combine results and basic stats into return object
  out <- list(
    results = res,
    mod_obj = mod_obj,
    data = input_data,
    sim = res_vpc,
    stats_summ = NULL,
    shrinkage = NULL,
    bayesian_impact = NULL
  )
  class(out) <- c("mipdeval_results", "list")

  # res is NULL when vpc_options(..., vpc_only = TRUE).
  if (!is.null(res)) {
    if(verbose) cli::cli_alert_info("Calculating forecasting statistics")
    out$stats_summ <- calculate_stats(out)
    out$shrinkage <- calculate_shrinkage(out)
    out$bayesian_impact <- calculate_bayesian_impact(out)
  }

  ## 5. Return results
  out
}
