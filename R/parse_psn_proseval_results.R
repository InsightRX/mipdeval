#' Parse PsN::proseval results.csv to filter out only the rows
#' that we need (for prediction of next level)
#'
#' @param data results data.frame or path to csv file
#' @param group group samples using the column name specified in `group`
#'
#' @returns A data.frame
#' @export
parse_psn_proseval_results <- function(data, group = NULL) {
  if(inherits(data, "character")) {
    res <- read.csv(file = data)
  } else {
    res <- data
  }
  res <- res |>
    dplyr::group_by(.data$ID) |>
    dplyr::filter(.data$EVID == 2 & .data$DV > 0)
  if(is.null(group)) { # assume we just want to look at next observation
    group <- "group"
    res <- res |>
      dplyr::group_by(.data$OBS, .add = TRUE) |>
      dplyr::mutate(group = seq(.data$OBS[1] + 1, .data$OBS[1] + dplyr::n(), 1)) |>
      dplyr::ungroup()
  }
  res <- res |>
    dplyr::filter(.data$group == .data$OBS + 1) |>
    dplyr::mutate(n = 1:length(.data$OBS)) |>
    dplyr::mutate(res = .data$DV - .data$IPRED) |>
    dplyr::select("TIME", obs = "DV", pred = "IPRED", pop = "PRED", "res", "n", !!group) |>
    dplyr::ungroup()
}
