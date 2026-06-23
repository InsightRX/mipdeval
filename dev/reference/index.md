# Package index

## Run iterative predictive analysis

All iterative predictive analyses begin with a call to
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
supplying a either a PKPDsim model object or library and a NONMEM-style
dataset.

- [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
  : Run iterative predictive analysis, looping over each individual's
  data

- [`plot(`*`<mipdeval_results>`*`)`](https://insightrx.github.io/mipdeval/dev/reference/plot.mipdeval_results.md)
  :

  Plot method for a
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
  object

## Set options for iterative predictive analysis

Pass these functions as arguments to
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
to set various options for iterative predictive analysis, including the
control of how and whether various summary statistics are calculated.

- [`fit_options()`](https://insightrx.github.io/mipdeval/dev/reference/fit_options.md)
  : Options for controlling MAP Bayesian fit
- [`bootstrap_options()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_options.md)
  : Options for bootstrapping error metrics
- [`stats_summ_options()`](https://insightrx.github.io/mipdeval/dev/reference/stats_summ_options.md)
  : Options for summary statistics
- [`vpc_options()`](https://insightrx.github.io/mipdeval/dev/reference/vpc_options.md)
  : Options for VPC simulations

## Calculate summary statistics for an iterative predictive analysis

These functions calculate various summary statistics from the results of
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md).
All of these calculations can be included in the outputs of
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
by setting various options; they are primarly exported here for
situations where additional post-processing is desired before running
calculations.

- [`calculate_bayesian_impact()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_bayesian_impact.md)
  : Calculate the impact of using Bayesian updating compared to
  population estimates
- [`calculate_shrinkage()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_shrinkage.md)
  : Calculate eta-shrinkage
- [`calculate_stats()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_stats.md)
  : Calculate basic statistics, like RMSE, MPE, MAPE for forecasted data
- [`calculate_bootstrap_summ()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_bootstrap_summ.md)
  : Bootstrap confidence intervals for forecasting error metrics

## Group observations in a NONMEM dataset

Helper functions for grouping observations in NONMEM datasets.

- [`add_grouping_column()`](https://insightrx.github.io/mipdeval/dev/reference/add_grouping_column.md)
  : Add grouping column using a function
- [`group_by_dose()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_dose.md)
  : Will create a separate group for each dose intervals that contains
  at least one sample
- [`group_by_time()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_time.md)
  : Group data by time using bin separators

## Built in datasets

- [`nm_busulfan`](https://insightrx.github.io/mipdeval/dev/reference/nm_busulfan.md)
  : Busulfan data
- [`nm_vanco`](https://insightrx.github.io/mipdeval/dev/reference/nm_vanco.md)
  : Vancomycin data

## Bootstrapping and error metrics

Lower-level functions for bootstrapping and calculating error metrics.

- [`bootstrap_metrics()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_metrics.md)
  [`summarise_bootstrap_metrics()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_metrics.md)
  [`summarize_bootstrap_metrics()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_metrics.md)
  : Calculate metrics across bootstrapped folds
- [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)
  [`is_accurate()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)
  [`is_accurate_abs()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)
  [`is_accurate_rel()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)
  : Accuracy
- [`mape()`](https://insightrx.github.io/mipdeval/dev/reference/mape.md)
  : Mean absolute percentage error
- [`mpe()`](https://insightrx.github.io/mipdeval/dev/reference/mpe.md) :
  Mean percentage error
- [`nrmse()`](https://insightrx.github.io/mipdeval/dev/reference/nrmse.md)
  : Normalized root-mean-squared error
- [`rmse()`](https://insightrx.github.io/mipdeval/dev/reference/rmse.md)
  : Root-mean-squared error

## Compare results with PsN

Helper functions to compare the results of
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
with the PsN `execute` and `proseval` tools.

- [`compare_psn_proseval_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md)
  [`reldiff_psn_proseval_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md)
  [`compare_psn_execute_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md)
  [`reldiff_psn_execute_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md)
  : Compare mipdeval results with PsN
- [`parse_psn_proseval_results()`](https://insightrx.github.io/mipdeval/dev/reference/parse_psn_proseval_results.md)
  : Parse PsN::proseval results.csv to filter out only the rows that we
  need (for prediction of next sample or group of samples)
