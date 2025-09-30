#' Core function for creating visual predictive checks (VPCs). Runs `n_samples`
#' simulations for a single subject
#'
#' @inheritParams run_eval
#' @inheritParams run_eval_core
#' @param n_samples Number of iterations to perform for VPC simulations.
#' @param seed Seed for random number generation.
#'
#' @returns data.frame
run_vpc_core <- function(
    data,
    mod_obj,
    progress_function,
    n_samples = 250,
    seed = 123
) {
  progress_function()

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
    seed = seed,
    only_obs = TRUE,
    verbose = FALSE
  )

  dat |>
    dplyr::mutate(
      iter = .data$id,
      id = data$id # data, not .data!
    ) |>
    dplyr::select(ID = "id", TIME = "t", DV = "y", ITER = "iter")
}

#' Options for VPC simulations
#'
#' @param ... These dots are reserved for future extensibility and must be empty.
#' @inheritParams run_vpc_core
#' @param skip Logical. Skip VPC simulations?
#' @param vpc_only Logical. Only return VPC simulations?
#'
#' @returns A list.
#' @export
vpc_options <- function(
  ...,
  n_samples = 250,
  seed = 123,
  skip = FALSE,
  vpc_only = FALSE
) {
  rlang::check_dots_empty()
  out <- list(
    n_samples = vctrs::vec_assert(
      n_samples, ptype = numeric(), size = 1L, arg = "n_samples"
    ),
    seed = seed,
    skip = vctrs::vec_assert(
      skip, ptype = logical(), size = 1L, arg = "skip"
    ),
    vpc_only = vctrs::vec_assert(
      vpc_only, ptype = logical(), size = 1L, arg = "vpc_only"
    )
  )
  structure(out, class = "mipdeval_vpc_options")
}
