# Options for bootstrapping error metrics

Configures bootstrapping of confidence intervals around the forecasting
error metrics. The result is passed to the `bootstrap` argument of
[`stats_summ_options()`](https://insightrx.github.io/mipdeval/dev/reference/stats_summ_options.md)
(which also supplies the accuracy error margins shared with the point
estimates). All metrics are bootstrapped; there is no metric selection
here.

## Usage

``` r
bootstrap_options(
  ...,
  n_boots = 1000,
  seed = 123,
  conf_level = 0.95,
  skip = TRUE
)
```

## Arguments

- ...:

  These dots are reserved for future extensibility and must be empty.

- n_boots:

  Number of bootstrapped samples to create (per group).

- seed:

  Single value for the random seed, used for reproducible random
  sampling.

- conf_level:

  The confidence level to use for the confidence interval. Must be
  strictly between 0 and 1. Defaults to a 95 percent confidence
  interval.

- skip:

  Logical. Skip bootstrapping? Defaults to `TRUE`, since bootstrapping
  can be expensive.

## Value

A list.
