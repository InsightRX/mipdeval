# Compare mipdeval results with PsN

The `compare_psn_*_results()` and `reldiff_psn_*_results()` family of
functions are designed to compare mipdeval results with PsN:

- `compare_psn_proseval_results()` and `compare_psn_execute_results()`
  summarize the relative difference in predictions between mipdeval and
  the PsN `execute` and `proseval` tools.

- `reldiff_psn_proseval_results()` and `reldiff_psn_execute_results`
  calculate the relative difference for each prediction between mipdeval
  and the PsN `execute` and `proseval` tools.

## Usage

``` r
compare_psn_proseval_results(mipdeval, psn, tol = 0.1)

reldiff_psn_proseval_results(mipdeval, psn)

compare_psn_execute_results(mipdeval, psn, tol = 0.1)

reldiff_psn_execute_results(mipdeval, psn)
```

## Arguments

- mipdeval:

  The
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
  object.

- psn:

  A PsN execute or proseval data set.

- tol:

  Optional. Tolerance.

## Value

A data/frame.
