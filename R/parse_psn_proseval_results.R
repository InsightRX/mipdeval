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
  res %>%
    dplyr::group_by(ID) %>%
    dplyr::filter(EVID == 2 & DV > 0) %>%
    dplyr::filter(!duplicated(OBS)) %>%
    dplyr::mutate(n = 1:length(OBS)) %>%
    dplyr::mutate(res = DV - IPRED) %>%
    dplyr::select(TIME, obs = DV, pred = IPRED, pop = PRED, res, n) %>%
    dplyr::ungroup()
}
