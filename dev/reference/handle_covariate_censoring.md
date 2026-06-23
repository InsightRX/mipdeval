# Handle covariate censoring

This removes new covariate information after a certain time cutoff

## Usage

``` r
handle_covariate_censoring(covariates, t, censor = TRUE)
```

## Arguments

- covariates:

  covariates data for single subject

- t:

  timepoint cutoff at which covariate are to be censored (if
  `censor=FALSE`)

- censor:

  censor covariate data?

## Value

list with similar shape as `covariates` object, just with potentially
censored future data.
