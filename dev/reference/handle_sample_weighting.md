# Handle weighting of samples

Binary selection of which samples are used in the fit (0 = excluded, 1 =
included), combined with optional continuous downweighting of older
samples via a time-based scheme (see
[`calculate_fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_fit_weights.md)).

## Usage

``` r
handle_sample_weighting(obs_data, iterations, incremental, i, weights = NULL)
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

- weights:

  optional sample downweighting scheme based on how long ago
  observations were taken. Preferably a
  [`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
  object (e.g.
  `fit_weights("weight_gradient_exponential", t12_decay = 72)`), but a
  string naming the scheme or a named list with a `scheme` element plus
  any scheme-specific parameters are also accepted. See
  [`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
  for all available schemes and their parameters. Default is `NULL` (no
  downweighting; all included samples are weighted equally).

## Value

vector of weights (numeric)
