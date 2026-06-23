# Parse PsN::proseval results.csv to filter out only the rows that we need (for prediction of next sample or group of samples)

Parse PsN::proseval results.csv to filter out only the rows that we need
(for prediction of next sample or group of samples)

## Usage

``` r
parse_psn_proseval_results(data, group = NULL)
```

## Arguments

- data:

  results data.frame or path to csv file

- group:

  optional. Group samples using the column name specified in `group`

## Value

A data.frame
