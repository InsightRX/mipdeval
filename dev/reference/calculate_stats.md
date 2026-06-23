# Calculate basic statistics, like RMSE, MPE, MAPE for forecasted data

Calculate basic statistics, like RMSE, MPE, MAPE for forecasted data

## Usage

``` r
calculate_stats(
  .res,
  rounding = 3,
  acc_error_abs = NULL,
  acc_error_rel = NULL,
  warn = TRUE
)
```

## Arguments

- .res:

  output object (`mipdeval_results`) from
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
  or `data.frame` with raw results.

- rounding:

  number of decimals to round to.

- acc_error_abs, acc_error_rel:

  For calculating
  [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md):
  Positive number providing an absolute or relative error margin. The
  cutoff is exclusive of the error margin. When `NULL` (the default),
  accuracy will not be calculated and will return `NA` instead.

- warn:

  should a warning be emitted when failed fits (NA predictions) are
  detected?

## Value

tibble
