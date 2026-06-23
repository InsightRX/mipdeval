# Add grouping column using a function

Add grouping column using a function

## Usage

``` r
add_grouping_column(data, fun = group_by_time, label = "group", ...)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- fun:

  Function used to define groups. Either (1) a quoted or character name
  referencing a function (e.g.,
  [`group_by_time()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_time.md),
  [`group_by_dose()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_dose.md),
  or a custom function), or (2) an anonymous function; see examples.

- label:

  column name of grouper column in dataset

- ...:

  Additional arguments passed onto `fun`. See the respective grouping
  functions for more details. E.g. for `group_by_time`, need a `bins`
  argument.

## Value

data.frame

## Details

All functions supplied to `add_grouping_column()` must take at least the
argument `data`, and return a numeric vector (or a vector that can be
cast to numeric) of the same length as the number of rows in `data`,
which will be used as the grouping column.

## See also

[`group_by_time()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_time.md),
[`group_by_dose()`](https://insightrx.github.io/mipdeval/dev/reference/group_by_dose.md)

## Examples

``` r
# group_by_dose:
add_grouping_column(nm_busulfan, fun = group_by_dose, label = "group")
#> # A tibble: 240 × 14
#>       ID  TIME   CMT     DV   AMT  EVID   MDV  RATE   AGE    WT    HT   SEX
#>    <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0       1    0      50     1     1  16.7     5    16   148     0
#>  2     1   3.5     1 1704.      0     0     0   0       5    16   148     0
#>  3     1   5       1 1084.      0     0     0   0       5    16   148     0
#>  4     1   6       1  496.      0     0     0   0       5    16   148     0
#>  5     1   8       1  261.      0     0     0   0       5    16   148     0
#>  6     1  12       1   56.1     0     0     0   0       5    16   148     0
#>  7     1  24       1    0      50     1     1  16.7     5    16   148     0
#>  8     1  27.5     1 1518.      0     0     0   0       5    16   148     0
#>  9     1  29       1 1075.      0     0     0   0       5    16   148     0
#> 10     1  30       1  779.      0     0     0   0       5    16   148     0
#> # ℹ 230 more rows
#> # ℹ 2 more variables: REGI <dbl>, group <int>

# group_by_time:
add_grouping_column(nm_busulfan, fun = "group_by_time", label = "group")
#> # A tibble: 240 × 14
#>       ID  TIME   CMT     DV   AMT  EVID   MDV  RATE   AGE    WT    HT   SEX
#>    <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0       1    0      50     1     1  16.7     5    16   148     0
#>  2     1   3.5     1 1704.      0     0     0   0       5    16   148     0
#>  3     1   5       1 1084.      0     0     0   0       5    16   148     0
#>  4     1   6       1  496.      0     0     0   0       5    16   148     0
#>  5     1   8       1  261.      0     0     0   0       5    16   148     0
#>  6     1  12       1   56.1     0     0     0   0       5    16   148     0
#>  7     1  24       1    0      50     1     1  16.7     5    16   148     0
#>  8     1  27.5     1 1518.      0     0     0   0       5    16   148     0
#>  9     1  29       1 1075.      0     0     0   0       5    16   148     0
#> 10     1  30       1  779.      0     0     0   0       5    16   148     0
#> # ℹ 230 more rows
#> # ℹ 2 more variables: REGI <dbl>, group <dbl>

# Anonymous function:
add_grouping_column(
  nm_busulfan,
  fun = function(data) {
    as.numeric(cut(
      data$TIME, breaks = c(0, 24, 48, 72, 96, Inf), include.lowest = TRUE
    ))
  },
  label = "group"
)
#> # A tibble: 240 × 14
#>       ID  TIME   CMT     DV   AMT  EVID   MDV  RATE   AGE    WT    HT   SEX
#>    <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0       1    0      50     1     1  16.7     5    16   148     0
#>  2     1   3.5     1 1704.      0     0     0   0       5    16   148     0
#>  3     1   5       1 1084.      0     0     0   0       5    16   148     0
#>  4     1   6       1  496.      0     0     0   0       5    16   148     0
#>  5     1   8       1  261.      0     0     0   0       5    16   148     0
#>  6     1  12       1   56.1     0     0     0   0       5    16   148     0
#>  7     1  24       1    0      50     1     1  16.7     5    16   148     0
#>  8     1  27.5     1 1518.      0     0     0   0       5    16   148     0
#>  9     1  29       1 1075.      0     0     0   0       5    16   148     0
#> 10     1  30       1  779.      0     0     0   0       5    16   148     0
#> # ℹ 230 more rows
#> # ℹ 2 more variables: REGI <dbl>, group <dbl>
```
