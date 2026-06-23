# Parse NONMEM-style input data, prepare for main eval loop

Parse NONMEM-style input data, prepare for main eval loop

## Usage

``` r
parse_input_data(data, covariates, ids, group = NULL, verbose = TRUE)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- covariates:

  vector of covariate names

- ids:

  optional, vector of subject IDs to run analysis on (by default runs
  analysis on all subjects in dataset)

- group:

  name of column in `data` that groups observations together in
  iterative flow. By default each observation will be its own "group",
  but this can be used to group peaks and troughs together, or to group
  observations on the same day together. Grouping will be done prior to
  running the analysis, so cannot be changed afterwards.

- verbose:

  show more output

## Value

list of lists, within each list regimen, observations, and covariates
