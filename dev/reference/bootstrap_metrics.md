# Calculate metrics across bootstrapped folds

Calculate metrics across bootstrapped folds of the data; optionally,
according to one or more grouped variables. Bootstrap size is equal to
the size of the data or each grouped variable, and rows are sampled with
replacement.

## Usage

``` r
bootstrap_metrics(.data, ..., .by = NULL, .seed = 10, .n_boots = 1000)

summarise_bootstrap_metrics(.data, .by = NULL, .conf_level = 0.95)

summarize_bootstrap_metrics(.data, .by = NULL, .conf_level = 0.95)
```

## Arguments

- .data:

  A data frame or data frame extension (e.g. a tibble).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions to compute across bootstrapped
  folds of the data. The name will be the name of the variable in the
  result.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, variables to group by. Each group is resampled
  independently.

- .seed:

  Single value for the random seed, used for reproducible random
  sampling.

- .n_boots:

  Number of bootstrapped samples to create (per group).

- .conf_level:

  The confidence level to use for the confidence interval. Must be
  strictly between 0 and 1. Defaults to a 95 percent confidence
  interval.

## Value

For `bootstrap_metrics()`: A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
whose columns are a combination of the summary expressions and grouping
keys that you provide, plus a `boot` column indicating each bootstrap
fold.

For `summarise_bootstrap_metrics()`: A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with, for each bootstrapped metric, three columns suffixed `_mid`,
`_lower`, and `_upper`, giving the mean and the lower and upper
quantiles implied by `.conf_level`, respectively.

## Useful metrics

- (Normalized) Root mean squared error:
  [`rmse()`](https://insightrx.github.io/mipdeval/dev/reference/rmse.md),
  [`nrmse()`](https://insightrx.github.io/mipdeval/dev/reference/nrmse.md)

- Mean (absolute) percent error:
  [`mpe()`](https://insightrx.github.io/mipdeval/dev/reference/mpe.md),
  [`mape()`](https://insightrx.github.io/mipdeval/dev/reference/mape.md)

- Accuracy:
  [`accuracy()`](https://insightrx.github.io/mipdeval/dev/reference/accuracy.md)

## Examples

``` r
set.seed(99)
df <- data.frame(
  observationid = rep(1:1000, 4),
  model = rep(c(rep("A", 1000), rep("B", 1000)), 2),
  patient_type = "general",
  prediction_type = c(rep("a priori", 2000), rep("a posteriori", 2000)),
  res = c(
    rnorm(1000, 2, 3),
    rnorm(1000, 0.1, 1),
    rnorm(1000, 1, 3),
    rnorm(1000, 0, 0.5)
  ),
  tdm = rnorm(4000, 10, 5)
)

boots <- bootstrap_metrics(
  df,
  rmse = rmse(tdm, tdm - res),
  nrmse = nrmse(tdm, tdm - res),
  accuracy = accuracy(tdm, tdm - res, 2.5, 0.2),
  .by = c(model, patient_type, prediction_type),
  .n_boots = 100
)

summarise_bootstrap_metrics(
  boots,
  .by = c(model, patient_type, prediction_type)
)
#> # A tibble: 4 × 12
#>   model patient_type prediction_type rmse_mid rmse_lower rmse_upper nrmse_mid
#>   <chr> <chr>        <chr>              <dbl>      <dbl>      <dbl>     <dbl>
#> 1 A     general      a posteriori       3.21       3.07       3.37     0.326 
#> 2 A     general      a priori           3.61       3.46       3.76     0.362 
#> 3 B     general      a posteriori       0.490      0.469      0.506    0.0487
#> 4 B     general      a priori           0.986      0.945      1.04     0.0972
#> # ℹ 5 more variables: nrmse_lower <dbl>, nrmse_upper <dbl>, accuracy_mid <dbl>,
#> #   accuracy_lower <dbl>, accuracy_upper <dbl>
```
