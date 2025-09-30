#' Do some checks and minor manipulations on input dataset
#'
#' E.g. confirm that all required columns are included, and column names are
#' all lower-case.
#'
#' @inheritParams run_eval
#'
#' @returns a data.frame
check_input_data <- function(data, dictionary, verbose = TRUE) {
  if(verbose) cli::cli_progress_step("Checking integrity of input data")
  if(!is.null(dictionary) && length(dictionary) > 0) {
    check_dictionary_columns(data, dictionary)
    check_valid_dictionary(dictionary)
    # TODO: We should move this into a different function like tidy_input_data().
    # Checks and data manipulation should occur separately.
    cli::cli_alert_info("Renaming dataset columns based on dictionary.")
    data <- data |>
      dplyr::rename(
        !!!rlang::set_names(
          dictionary,
          names(dictionary)
        )
      )
  } else {
    cli::cli_alert_info("No data `dictionary` provided, assuming common NONMEM column names in input dataset.")
  }
  check_required_cols(data)
  if(verbose) cli::cli_progress_done()
  data
}

check_dictionary_columns <- function(data, dictionary) {
  has_cols <- unlist(dictionary) %in% names(data)
  if(all(has_cols)) return()
  missing_cols <- unlist(dictionary)[!has_cols]
  cli::cli_abort(c(
           "Column names in {.arg dictionary} must exist in {.arg data}.",
    "x" = "{.str {missing_cols}} {?is/are} not {?a/} column{?s} in {.arg data}."
  ))
}

check_valid_dictionary <- function(dictionary) {
  valid_names <- c("ID", "TIME", "EVID", "DV")
  has_valid_names <- names(dictionary) %in% valid_names
  if (all(has_valid_names)) return()
  invalid_names <- names(dictionary)[!has_valid_names]
  cli::cli_abort(c(
    "{.arg dictionary} names must be any of the following: {.field {valid_names}}.",
    "x" = "{.field {invalid_names}} {?is/are} not {?a/} valid name{?s}.",
    "i" = "Did you make a typo?"
  ))
}

check_required_cols <- function(data) {
  required_cols <- c("ID", "TIME", "EVID", "DV")
  has_required <- required_cols %in% names(data)
  if (all(has_required)) return()
  missing_cols <- required_cols[!has_required]
  cli::cli_abort(c(
          "{.arg data} must have the following columns: {.field {required_cols}}.",
    "x" = "{.field {missing_cols}} {?is/are} missing.",
    "i" = "Did you forget to rename a column?"
  ))
}


