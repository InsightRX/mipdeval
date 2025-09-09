#' Parse PsN::proseval results.csv to filter out only the rows
#' that we need (for prediction of next level)
#'
#' @param data results data.frame or path to csv file
#'
#' @export
#'
parse_psn_proseval_results <- function(data) {
  if(inherits(data, "character")) {
    res <- read.csv(file = data)
  } else {
    res <- data
  }
  res |>
    dplyr::group_by(.data$ID) |>
    dplyr::filter(.data$EVID == 2 & .data$DV > 0) |>
    dplyr::filter(!duplicated(.data$OBS)) |>
    dplyr::mutate(n = 1:length(.data$OBS)) |>
    dplyr::mutate(res = .data$DV - .data$IPRED) |>
    dplyr::select("TIME", obs = "DV", pred = "IPRED", pop = "PRED", "res", "n") |>
    dplyr::ungroup()
}
