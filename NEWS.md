# mipdeval (development version)

* Added support for bootstrapping error metrics for forecasted data: `run_eval()` now returns a `bootstrap_summ` element with point and 95% confidence interval estimates for each error metric (e.g., RMSE, MPE, etc.). Bootstrapping is configured through the new `bootstrap` argument of `stats_summ_options()` (see `bootstrap_options()`; off by default). The lower-level `bootstrap_metrics()` and `summarise_bootstrap_metrics()` functions, as well as `calculate_bootstrap_summ()`, are also exported for general use. (#47)

* Added `accuracy()` statistic for calculating the proportion of predicted drug concentrations that fall within specified absolute and relative error margins. This statistic will only be calculated in `run_eval()` when the absolute and relative error margins have been specified in the `.stats_summ_options` argument (see `stats_summ_options()`). (#30)

# mipdeval 0.1.0

* Initial version.
