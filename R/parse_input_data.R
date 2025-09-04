#' Parse NONMEM-style input data, prepare for main eval loop
#'
#' @param data NONMEM-style input datataset, as data.frame
#' @param covariates vector of covariate names
#'
#' @returns list of lists, within each list regimen, observations, and
#' covariates
#'
parse_input_data <- function(
    data,
    covariates
) {

  ## Split data into individual data.frames by ID
  new_data <- data |>
    dplyr::group_by(ID) |>
    dplyr::group_split()

  ## Parse each data.frame into a list with regimen, observations, and covariates
  out <- purrr::map(
    new_data,
    parse_nm_data,
    covariates = covariates
  )

  out
}

parse_nm_data <- function(data, covariates) {
  out <- list()
  out$covariates <- make_covariates_object(data, covariates)
  out$regimen <- PKPDsim::nm_to_regimen(data)
  out$observations <- data |>
    dplyr::filter(EVID == 0) |>
    dplyr::select(ID, TIME, DV, CMT) |>
    dplyr::rename(id = ID, t = TIME, y = DV, cmt = CMT)
  out
}

make_covariates_object <- function(data, covariates) {
  obs_data <- dplyr::filter(data, EVID == 0)
  obs_times <- obs_data$TIME
  covariates_data <- as.list(dplyr::select(obs_data, dplyr::all_of(covariates)))
  out <- purrr::map(
    covariates_data,
    function(.x) {
      if(length(unique(.x)) == 1) {
        PKPDsim::new_covariate(.x[1])
      } else { # time-varying
        PKPDsim::new_covariate(.x, times = obs_times)
      }
    }
  )
  out
}
