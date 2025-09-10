#' Parse PKPDsim model information
#'
#' Parse model information from PKPDsim model object or an installed PKPDsim
#' model library.
#'
#' @inheritParams run_eval
#' @inheritParams rlang::args_dots_used
#'
#' @returns A named list.
parse_model <- function(model, ...) {
  rlang::check_dots_used()
  UseMethod("parse_model")
}

#' @export
parse_model.default <- function(model, ...) {
  cli::cli_abort(c(
    paste0(
      "{.arg model} must be a PKPDsim model object or ",
      "a character string of an installed PKPDsim model library."
    ),
    "x" = "You've supplied an object of class {.cls {class(model)}}."
  ))
}

#' @export
#' @rdname parse_model
parse_model.character <- function(
  model,
  ...,
  parameters = NULL,
  ruv = NULL,
  omega = NULL,
  iov_bins = NULL
) {
  check_installed_model_library(model)
  withr::with_package(model, quietly = TRUE, {
    # Use model defaults if user has not supplied overrides:
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(ruv)) ruv <- ruv()
    if (is.null(omega)) omega <- omega_matrix()
    if (is.null(iov_bins)) iov_bins <- iov()
    list(
      model = model(),
      parameters = parameters,
      # covariates = PKPDsim::get_model_covariates(model()),
      ruv = ruv,
      omega_matrix = omega,
      iov_bins = iov_bins,
      fixed = fixed()
    )
  })
}

#' @export
#' @rdname parse_model
parse_model.PKPDsim <- function(
  model,
  ...,
  parameters,
  ruv,
  omega,
  iov_bins
) {
  list(
    model = model,
    parameters = parameters,
    ruv = ruv,
    omega_matrix = omega,
    iov_bins = iov_bins
  )
}

#' Check if PKPDsim model library is installed
#'
#' @param model Character. Name of an installed PKPDsim model library.
#' @inheritParams cli::cli_abort
#'
#' @returns This function is called for its side effects and returns `NULL` if
#'   the PKPDsim model library is installed or returns an error otherwise.
check_installed_model_library <- function(model, call = rlang::caller_env()) {
  rlang::try_fetch(
    rlang::with_interactive(rlang::check_installed(model), value = FALSE),
    error = function(cnd) cli::cli_abort(
      message = c(
        conditionMessage(cnd),
        i = paste0(
          "Have you forgotten to compile the model to an R package using ",
          "{.fun PKPDsim::model_from_api}?"
        )
      ),
      call = call
    )
  )
}
