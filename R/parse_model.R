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
  fixed = NULL,
  iov = NULL
) {
  check_installed_model_library(model)

  withr::with_package(model, quietly = TRUE, {
    # Use model defaults if user has not supplied overrides:
    if (is.null(parameters)) parameters <- parameters()
    if (is.null(ruv)) ruv <- ruv()
    if (is.null(omega)) omega <- omega_matrix()
    if (is.null(fixed)) fixed <- fixed()
    if (is.null(iov)) iov <- iov()
    bins <- as.numeric(iov$bins)
    out <- list(
      model = model(),
      parameters = parameters,
      ruv = ruv,
      omega = omega,
      fixed = fixed(),
      bins = bins
    )
    if(!is.null(iov)) {
      iov_obj <- PKPDmap::create_iov_object(
        cv = iov$cv,
        omega = omega,
        bins = bins,
        parameters = parameters,
        fixed = fixed,
        ruv = ruv,
        verbose = FALSE
      )
      iov_updates <- c("parameters", "omega", "fixed", "bins")
      out[iov_updates] <- iov_obj[iov_updates]
    }
  })
  out
}

#' @export
#' @rdname parse_model
parse_model.PKPDsim <- function(
  model,
  ...,
  parameters,
  ruv,
  omega,
  fixed = NULL,
  iov = NULL
) {
  out <- list(
    model = model,
    parameters = parameters,
    ruv = ruv,
    omega = omega,
    fixed = fixed,
    bins = numeric(0)
  )
  if(!is.null(iov)) {
    iov_obj <- PKPDmap::create_iov_object(
      cv = iov$cv,
      omega = omega,
      bins = as.numeric(iov$bins),
      parameters = parameters,
      fixed = fixed,
      ruv = ruv,
      verbose = FALSE
    )
    iov_updates <- c("parameters", "omega", "fixed", "bins")
    out[iov_updates] <- iov_obj[iov_updates]
  }
  out
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
