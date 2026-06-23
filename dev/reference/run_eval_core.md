# Core iterative simulation and MAP estimation function that loops over an individual's dataset

Core iterative simulation and MAP estimation function that loops over an
individual's dataset

## Usage

``` r
run_eval_core(
  mod_obj,
  data,
  weights = NULL,
  weight_prior = 1,
  censor_covariates = TRUE,
  incremental = FALSE,
  progress_function = function() {
 },
  .fit_options = NULL
)
```

## Arguments

- mod_obj:

  list object with model information

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- weights:

  vector of weights for error. Length of vector should be same as length
  of observation vector. If NULL (default), all weights are equal. Used
  in both MAP and NP methods. Note that \`weights\` argument will also
  affect residuals (residuals will be scaled too).

- weight_prior:

  weighting of priors in relationship to observed data, default = 1

- censor_covariates:

  with the `proseval` tool in PsN, there is “data leakage” (of future
  covariates data): since the NONMEM dataset in each step contains the
  covariates for the future, this is technically data leakage, and could
  result in an over-optimistic estimate of predictive performance. In
  `mipdeval`, covariate censoring of future covariate data is switched
  on by default (so no data leakage), but it can be switched off if we
  want to match the behavior of `PsN::proseval` exactly.

- incremental:

  should MAP Bayesian do incremental fits in the iterative loop? I.e. in
  this case it would use the first iterations MAP Bayesian estimates as
  input for the second iteration, and so forth. The uncertainty around
  the MAP estimates would be used as the new `omega` matrix. This
  approach has been called "model predictive control (MPC)"
  (www.page-meeting.org/?abstract=9076) and may be more predictive than
  "regular" MAP in some scenarios. Default is `FALSE`.

- progress_function:

  function to increment progress bar

- .fit_options:

  Options for controlling MAP Bayesian fit. This must be the result from
  a call to
  [`fit_options()`](https://insightrx.github.io/mipdeval/dev/reference/fit_options.md).

## Value

a `data.frame` with individual predictions
