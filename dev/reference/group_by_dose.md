# Will create a separate group for each dose intervals that contains at least one sample

Will create a separate group for each dose intervals that contains at
least one sample

## Usage

``` r
group_by_dose(
  data,
  dictionary = list(ID = "ID", EVID = "EVID", TIME = "TIME"),
  ...
)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

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
group_by_dose(nm_busulfan)
#>   [1] 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3
#>  [38] 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1
#>  [75] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3
#> [112] 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1
#> [149] 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3
#> [186] 3 4 4 4 4 4 4 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 1 1 1 1 1 1
#> [223] 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4
```
