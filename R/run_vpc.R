#' Run iterative predictive analysis, looping over each individual's data
#' to create observation and simulation datasets for creating visual predictive
#' checks (VPCs).
#'
#' @inheritParams run_eval
#' @param n_samples number of iterations to perform for VPC simulations.
#'
#' @returns A named list of data frames with `obs` and `sim` data.
#'
#' @exapmles
#' \dontrun{
#' library(mipdeval)
#' library(vpc)
#' vpc_vanc <- run_vpc(
#'   model = "pkvancothomson",
#'   data = nm_vanco,
#'   n_samples = 100,
#'   progress = FALSE
#' )
#' vpc(
#'   sim = vpc_vanc$sim,
#'   obs = vpc_vanc$obs
#' )
#' }
#'
#' @export
run_vpc <- function(
    model,
    data,
    ids = NULL,
    n_samples = 250,
    parameters = NULL,
    fixed = NULL,
    omega = NULL,
    iov = NULL,
    ruv = NULL,
    dictionary = list(),
    group = NULL,
    progress = TRUE,
    threads = 1,
    verbose = TRUE
) {

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
  if(progress) {
    progressr::handlers(global = TRUE)
    progressr::handlers("cli")
    p <- progressr::progressor(along = data_parsed)
  } else {
    p <- function() { }
  }

  ## 3. run the core function on each individual-level dataset in the list
  if(verbose) cli::cli_alert_info("Running forecasts for subjects in dataset")
  if(threads > 1) {
    future::plan(future::multisession, workers = threads)
    res <- furrr::future_map(
      .x = data_parsed,
      .f = run_eval_core,
      mod_obj = mod_obj,
      n_samples = n_samples,
      progress_function = p
    )
  } else {
    res <- purrr::map(
      .x = data_parsed,
      .f = run_vpc_core,
      mod_obj = mod_obj,
      n_samples = n_samples,
      progress_function = p
    )
  }
  sim_data <- dplyr::bind_rows(res) |>
    dplyr::select(ID = .data$id, TIME = .data$t, DV = .data$y, ITER = .data$iter) |>
    dplyr::arrange(.data$ITER, .data$ID, .data$TIME)

  out <- list(
    obs = input_data |>
      dplyr::filter(.data$EVID == 0),
    sim = sim_data
  )

  out

}

#' Core function for `run_vpc`. Runs `n_samples` simulations for
#' a single subject
#'
#' @inheritParams run_vpc
#' @inheritParams run_eval_core
#'
#' @returns data.frame
run_vpc_core <- function(
    data,
    mod_obj,
    progress_function,
    n_samples = 250
) {

  progress_function()

  obs_data <- data$observations
  comb <- data.frame()

  ## Simulate using PKPDsim
  bins <- NULL
  if(length(mod_obj$bins) > 0) {
    bins <- mod_obj$bins
  }
  dat <- PKPDsim::sim(
    ode = mod_obj$model,
    parameters = mod_obj$parameters,
    omega = mod_obj$omega,
    res_var = mod_obj$ruv,
    t_obs = data$observations$t,
    covariates = data$covariates,
    regimen = data$regimen,
    iov_bins = bins,
    n_ind = n_samples,
    only_obs = TRUE,
    verbose = FALSE
  ) |>
    dplyr::mutate(
      "iter" = .data$id,
      "id" = data$id # data, not .data!
    )

  dat
}
