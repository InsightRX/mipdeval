#' Read input data
#'
#' @inheritParams run_eval
#'
#' @returns data.frame
#'
read_input_data <- function(data) {
  if(inherits(data, "character")) {
    if(!file.exists(data)) {
      cli::cli_abort("Sorry, filename supplied to `data` does not exist.")
    }
    input_data <- read.csv(file = data)
  } else if(inherits(data, "data.frame")) {
    input_data <- data
  } else {
    cli::cli_abort("Sorry, object supplied to `data` argument should be either a filename or a data.frame.")
  }
  input_data
}
