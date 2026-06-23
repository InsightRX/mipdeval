# Changelog

## mipdeval (development version)

- Added support for bootstrapping error metrics for forecasted data:
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
  now returns a `bootstrap_summ` element with point and 95% confidence
  interval estimates for each error metric (e.g., RMSE, MPE, etc.).
  Bootstrapping is configured through the new `bootstrap` argument of
  [`stats_summ_options()`](https://insightrx.github.io/mipdeval/dev/reference/stats_summ_options.md)
  (see
  [`bootstrap_options()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_options.md);
  off by default). The lower-level
  [`bootstrap_metrics()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_metrics.md)
  and
  [`summarise_bootstrap_metrics()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_metrics.md)
  functions, as well as
  [`calculate_bootstrap_summ()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_bootstrap_summ.md),
  are also exported for general use.
  ([\#47](https://github.com/InsightRX/mipdeval/issues/47))

- Added
  [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)
  statistic for calculating the proportion of predicted drug
  concentrations that fall within specified absolute and relative error
  margins. This statistic will only be calculated in
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
  when the absolute and relative error margins have been specified in
  the `.stats_summ_options` argument (see
  [`stats_summ_options()`](https://insightrx.github.io/mipdeval/dev/reference/stats_summ_options.md)).
  ([\#30](https://github.com/InsightRX/mipdeval/issues/30))

## mipdeval 0.1.0

- Initial version.
