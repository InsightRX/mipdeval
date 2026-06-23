# Group data by time using bin separators

Group data by time using bin separators

## Usage

``` r
group_by_time(
  data,
  bins = c(0, 24, 48, 72, 96, Inf),
  dictionary = list(TIME = "TIME"),
  ...
)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- bins:

  vector of bin separators. Suggestion to keep the last bin separator
  `Inf`, to ensure all points are included in a group.

- dictionary:

  data dictionary. Optional, a named character vector that specifies the
  column names to be used from the dataset.

- ...:

  Additional arguments passed onto `fun`. See the respective grouping
  functions for more details. E.g. for `group_by_time`, need a `bins`
  argument.

## Value

a numeric vector of the same length as the number of rows in `data`.

## Examples

``` r
group_by_time(nm_busulfan)
#>   [1] 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2
#>  [38] 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1
#>  [75] 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3
#> [112] 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1
#> [149] 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3
#> [186] 3 3 4 4 4 4 4 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 1 1 1 1 1 1
#> [223] 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4
```
