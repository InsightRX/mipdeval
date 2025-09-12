#' Parse model info based on user-supplied info and potentially PKPDsim library
#'
#' @inheritParams run_eval
#'
parse_model_info <- function(
  model,
  parameters,
  omega,
  ruv,
  iov_bins
) {
  if(inherits(model, "character")) {
    args <- as.list(environment())
    args$model <- NULL
    load_from_lib <- c("model", "parameters", "ruv", "omega_matrix", "iov", "fixed")
    suppressMessages( ## avoid message "the following objects are masked from ..."
      require(model, character.only = TRUE)
    )
    mod_obj <- list()
    for(key in load_from_lib) {
      if(!is.null(args[[key]])) {
        cli::cli_alert_info("Will use user-supplied `{key}`, overriding model default.")
        mod_obj[[key]] <- args[[key]]
      } else {
        mod_obj[[key]] <- get(key, asNamespace(model))()
      }
    }
    if(!is.null(mod_obj$iov)) {
      iov_obj <- PKPDmap::create_iov_object(
        cv = mod_obj$iov$cv,
        omega = mod_obj$omega_matrix,
        bins = mod_obj$iov$bins,
        parameters = mod_obj$parameters,
        fixed = mod_obj$fixed,
        ruv = mod_obj$ruv,
        verbose = FALSE
      )
      ## Update model object wit IOV updates
      mod_obj[c("parameters", "omega_matrix", "kappa", "fixed", "bins")] <- iov_obj[
        c("parameters", "omega", "kappa", "fixed", "bins")
      ]
    }
  } else if (inherits(model, "PKPDsim")) {
    if(is.null(parameters) || is.null(omega) || is.null(ruv)) {
      cli::cli_abort("When specify a PKPDsim model as the `model` argument, also need at least the arguments `parameters`, `omega`, and `ruv`.")
    }
    mod_obj <- list(
      model = model,
      parameters = parameters,
      omega_matrix = omega,
      fixed = attr(model, "fixed"),
      ruv = ruv,
      iov_bins = iov_bins
    )
  } else {
    cli::cli_abort("`model` argument needs to be either a PKPDsim model, or a string pointing to a PKPDsim model library.")
  }

  mod_obj
}
