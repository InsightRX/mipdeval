#' Calculate eta-shrinkage
#'
#' Calculate eta-shrinkage, measure of how much information is available to
#' update individual estimates away from the population value.
#'
#' @inheritParams run_eval_core
#' @param res_df data.frame or tibble with raw results (including parameters)
#' from a run with `mipdeval::run_eval()`.
#'
#' @returns tibble
#' @export
calculate_shrinkage <- function(
  res_df,
  mod_obj
) {
  om <- get_omega_for_parameters(mod_obj)
  shr_df <- res_df |>
    dplyr::mutate(dplyr::across(
      .cols = names(om),
      .fns  = ~ calc_eta(.x, dplyr::cur_column(), mod_obj$parameters),
      .names = "eta_{.col}"
    )) |>
    dplyr::group_by(.data$`_iteration`) |>
    dplyr::summarise(dplyr::across(
      .cols = paste0("eta_", names(om)),
      .fns = ~ calc_shrinkage(.x, dplyr::cur_column(), om),
      .names = "shr_{.col}"
    ))
  names(shr_df) <- gsub("^shr_eta_", "", names(shr_df))
  shr_df
}

#' Calculate the "eta"-value for a parameters, assuming an exponential
#' shape for IIV, i.e. `PAR = TV_PAR * EXP(ETA(n))`, and
#' additive shape for IOV (kappa's), i.e. `PAR = TV_PAR + ETA(n)` or when
#' final parameter estimate is 0.
#'
#' @param ind individual parameter estimate
#' @param par_name name of parameter in model
#' @param parameters list of parameter values, should include entry for
#'   `par_name`
calc_eta <- function(ind, par_name, parameters) {
  pop <- parameters[[par_name]]
  if(grepl("^kappa_", par_name) || pop == 0) { ## PAR = TV_PAR + ETA()
    return(ind - pop)
  } else {
    return(log(ind/pop)) ## PAR = TV_PAR + ETA()
  }
}

calc_shrinkage <- function(eta, par_name, om) {
  100 * (1 - (stats::sd(eta) / sqrt(om[[gsub("^eta_", "", par_name)]])))
}

#' Return all diagonal om^2 elements for each non-fixed parameter, as a list
#'
#' @inheritParams run_eval_core
#'
#' @returns list
get_omega_for_parameters <- function(mod_obj) {
  pars <- names(mod_obj$parameters)
  pars <- pars[! (pars %in% mod_obj$fixed)]
  om <- as.list(
    diag(
      PKPDsim::triangle_to_full(mod_obj$omega)
    )[1:length(pars)]
  )
  names(om) <- pars
  om
}
