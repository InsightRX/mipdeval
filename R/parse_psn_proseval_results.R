#' Parse PsN::proseval results.csv to filter out only the rows
#' that we need (for prediction of next sample or group of samples)
#'
#' @param data results data.frame or path to csv file
#' @param group optional. Group samples using the column name specified in
#'   `group`
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
  ## create a data.frame on which OBS iterations to take from proseval results
  idx_tab <- res |>
    dplyr::filter(OBS == 1) |>
    dplyr::group_by(.data$ID,) |>
    dplyr::mutate(idx = 1:dplyr::n()) |>
    dplyr::filter(!duplicated(group)) |>
    dplyr::ungroup() |>
    dplyr::filter(group > 1) |>
    dplyr::select(ID, idx) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(idx = list(idx))
  dplyr::left_join(res, idx_tab) |>
    dplyr::filter(purrr::map2_lgl(idx, OBS, ~ .y %in% .x)) |>  # filter rows that match the index table
    dplyr::group_by(ID, OBS) |>
    dplyr::filter(group == group[1]) |>
    dplyr::mutate(res = .data$DV - .data$IPRED) |>
    dplyr::select("TIME", obs = "DV", pred = "IPRED", pop = "PRED", "res", !!group) |>
    dplyr::ungroup()
}
