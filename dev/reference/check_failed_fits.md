# Check for failed fits / predictions and (optionally) warn

Detects predictions that came back as `NA`, which indicates the
underlying MAP Bayesian fit failed, and—when `warn = TRUE`—emits a
warning summarising how many predictions failed and in which subjects.

## Usage

``` r
check_failed_fits(.res)
```

## Arguments

- .res:

  output object (`mipdeval_results`) from
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
  or a `data.frame` with raw results.

## Value

invisibly, a `data.frame` of the rows with failed predictions.
