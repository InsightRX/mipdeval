# Bootstrap confidence intervals for forecasting error metrics

Bootstraps point and confidence interval estimates of forecasting error
metrics from the results of
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md).
This is the engine behind the `bootstrap_summ` element of a
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
result; it is exported so that the bootstrap summary can be
(re)computed, for example with additional grouping variables, without
re-running the analysis.

## Usage

``` r
calculate_bootstrap_summ(
  .res,
  acc_error_abs = NULL,
  acc_error_rel = NULL,
  n_boots = 1000,
  seed = 123,
  conf_level = 0.95,
  .by = NULL
)
```

## Arguments

- .res:

  output object (`mipdeval_results`) from
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
  or a `data.frame` with raw results (the `results` element).

- acc_error_abs, acc_error_rel:

  Positive number providing an absolute or relative error margin for the
  accuracy metric. See
  [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md).
  Accuracy is bootstrapped only when both are supplied.

- n_boots:

  Number of bootstrapped samples to create (per group).

- seed:

  Single value for the random seed, used for reproducible random
  sampling.

- conf_level:

  The confidence level to use for the confidence interval. Must be
  strictly between 0 and 1. Defaults to a 95 percent confidence
  interval.

- .by:

  Optional vector of additional columns to group by, on top of
  `apriori`.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per `apriori` (and `.by`) group and, for each metric,
columns suffixed `_mid`, `_lower`, and `_upper`.

## Details

The following error metrics are bootstrapped: RMSE, NRMSE, MPE, and
MAPE, plus accuracy when both error margins are supplied.

For each row, the prediction that is evaluated depends on whether the
prediction is *a priori* or *a posteriori*: a priori predictions are
evaluated against the population prediction (`pred`) and a posteriori
predictions against the iterative individual prediction (`iter_ipred`).
Metrics are always grouped by `apriori`; additional grouping columns can
be supplied via `.by`.
