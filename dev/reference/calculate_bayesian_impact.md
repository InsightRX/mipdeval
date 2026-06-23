# Calculate the impact of using Bayesian updating compared to population estimates

Bayesian impact (BI), as defined here, is the gain in accuracy that is
obtained when using Bayesian updating, compared to using a priori
(population) parameter estimates. The BI is computed based on accuracy
measures (RMSE and MAPE), but not on bias estimates (MPE).

## Usage

``` r
calculate_bayesian_impact(.res)
```

## Arguments

- .res:

  output object (`mipdeval_results`) from
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
  or `data.frame` with raw results.

## Value

a tibble with bayesian impact values, based on RMSE and MAPE
