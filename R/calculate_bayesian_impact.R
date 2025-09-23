#' This function calculates the impact of using Bayesian updating, compared
#' to using population estimates
#'
#' Bayesian impact (BI), as defined here, is the gain in accuracy that is
#' obtained when using Bayesian updating, compared to using a priori
#' (population) parameter estimates. The BI is computed based on accuracy
#' measures (RMSE and MAPE), but not on bias estimates (MPE).
#'
calculate_bayesian_impact <- function(
  stats_summ
) {
  stats_summ |>
    dplyr::filter(!apriori) |>
    dplyr::select(type, apriori, rmse, mape) |>
    tidyr::pivot_wider(names_from = "type", values_from = c("rmse", "mape")) |>
    dplyr::summarise(
      bi_rmse = 100 * (1 - (rmse_iter_ipred / rmse_pred)),
      bi_mape = 100 * (1 - (mape_iter_ipred / mape_pred))
    )
}
