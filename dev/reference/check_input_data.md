# Do some checks and minor manipulations on input dataset

E.g. confirm that all required columns are included, and column names
are all lower-case.

## Usage

``` r
check_input_data(data, dictionary, verbose = TRUE)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- dictionary:

  data dictionary. Optional, a named character vector that specifies the
  column names to be used from the dataset.

- verbose:

  show more output

## Value

a data.frame
