#' Calculate the impact of using Bayesian updating compared to population estimates
#'
#' Bayesian impact (BI), as defined here, is the gain in accuracy that is
#' obtained when using Bayesian updating, compared to using a priori
#' (population) parameter estimates. The BI is computed based on accuracy
#' measures (RMSE and MAPE), but not on bias estimates (MPE).
#'
#' @param stats_summ stats summary, a data.frame created in `run_eval()`
#'
#' @returns a tibble with bayesian impact values, based on RMSE and MAPE
#'
#' @export
calculate_bayesian_impact <- function(
  stats_summ
) {
  stats_summ |>
    dplyr::filter(! .data$apriori) |>
    dplyr::select("type", "apriori", "rmse", "mape") |>
    tidyr::pivot_wider(names_from = "type", values_from = c("rmse", "mape")) |>
    dplyr::summarise(
      bi_rmse = 100 * (1 - (.data$rmse_iter_ipred / .data$rmse_pred)),
      bi_mape = 100 * (1 - (.data$mape_iter_ipred / .data$mape_pred))
    )
}
