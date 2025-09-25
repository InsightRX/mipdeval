#' Calculate basic statistics, like RMSE, MPE, MAPE for forecasted data
#'
#' @param res output object (`mipdeval_results`) from `run_eval()`, or
#' `data.frame` with raw results.
#' @param rounding number of decimals to round to.
#'
#' @returns tibble
#'
#' @export
calculate_stats <- function(
    res,
    rounding = 3
) {
  if(inherits(res, "mipdeval_results")) {
    res <- res$results
  }
  out <- res |>
    tidyr::pivot_longer(
      cols = c("pred", "map_ipred", "iter_ipred"), names_to = "type"
    ) |>
    dplyr::group_by(.data$type, .data$apriori) |>
    dplyr::summarise(
      rmse = rmse(.data$dv, .data$value),
      nrmse = nrmse(.data$dv, .data$value),
      mpe = mpe(.data$dv, .data$value),
      mape = mape(.data$dv, .data$value)
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), round, rounding)) |>
    dplyr::as_tibble()
  class(out) <- c("mipdeval_results_stats_summ", class(out))
  out
}
