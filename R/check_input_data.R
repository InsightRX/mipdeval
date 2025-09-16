#' Do some checks and minor manipulations on input dataset
#'
#' E.g. confirm that all required columns are included, and column names are
#' all lower-case.
#'
#' @inheritParams run_eval
#'
#' @returns a data.frame
check_input_data <- function(data, dictionary) {
  if(!is.null(dictionary) && length(dictionary) > 0) {
    if(!all(unlist(dictionary) %in% names(data))) {
      cli::cli_abort("Not all columns listed in dictionary were found in input data.")
    }
    cli::cli_alert_info("Renaming dataset columns based on dictionary.")
    valid_dict <- c("ID", "TIME", "EVID", "DV")
    idx <- names(dictionary) %in% valid_dict
    if(any(! idx)) {
      names(dictionary)[!idx]
      cli::cli_abort("Dictionary entries not recognized: {names(dictionary)[!idx]}. Valid data dictionary entries are: {valid_dict}")
    }
    data <- data |>
      dplyr::rename(
        !!!rlang::set_names(
          dictionary,
          names(dictionary)
        )
      )
  } else {
    cli::cli_alert_info("No data `dictionary` provided, assuming common NONMEM column names.")
  }
  data
}
