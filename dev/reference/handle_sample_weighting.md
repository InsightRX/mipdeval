# Handle weighting of samples

This function is used to select the samples used in the fit (1 or 0),
but also to select their weight, if a sample weighting strategy is
selected.

## Usage

``` r
handle_sample_weighting(obs_data, iterations, incremental, i)
```

## Arguments

- obs_data:

  tibble or data.frame with observed data for individual

- iterations:

  numeric vector of groups

- incremental:

  should MAP Bayesian do incremental fits in the iterative loop? I.e. in
  this case it would use the first iterations MAP Bayesian estimates as
  input for the second iteration, and so forth. The uncertainty around
  the MAP estimates would be used as the new `omega` matrix. This
  approach has been called "model predictive control (MPC)"
  (www.page-meeting.org/?abstract=9076) and may be more predictive than
  "regular" MAP in some scenarios. Default is `FALSE`.

- i:

  index

## Value

vector of weights (numeric)
