#' Do some checks and minor manipulations on input dataset
#'
#' E.g. confirm that all required columns are included, and column names are
#' all lower-case.
#'
#' @inheritParams run_eval
#'
#' @returns a data.frame
#'
check_input_data <- function(data, dictionary) {
  if(!is.null(dictionary) && length(dictionary) > 0) {
    if(!all(unlist(dictionary) %in% names(data))) {
      cli::cli_abort("Not all columns listed in dictionary were found in input data.")
    }
    cli::cli_alert_info("Renaming dataset columns based on dictionary.")
    new_data <- data %>%
      rename(!!!setNames(unlist(dictionary), names(dictionary)))
  } else {
    cli::cli_alert_info("No data `dictionary` provided, assuming common NONMEM column names.")
  }
  data
}
