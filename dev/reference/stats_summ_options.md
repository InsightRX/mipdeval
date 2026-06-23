# Options for summary statistics

Options for summary statistics

## Usage

``` r
stats_summ_options(
  ...,
  rounding = 3,
  acc_error_abs = NULL,
  acc_error_rel = NULL,
  bootstrap = bootstrap_options()
)
```

## Arguments

- ...:

  These dots are reserved for future extensibility and must be empty.

- rounding:

  number of decimals to round to.

- acc_error_abs, acc_error_rel:

  For calculating
  [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md):
  Positive number providing an absolute or relative error margin. The
  cutoff is exclusive of the error margin. When `NULL` (the default),
  accuracy will not be calculated and will return `NA` instead.

- bootstrap:

  Options for bootstrapping confidence intervals around the summary
  statistics. This must be the result from a call to
  [`bootstrap_options()`](https://insightrx.github.io/mipdeval/dev/reference/bootstrap_options.md).

## Value

A list.
