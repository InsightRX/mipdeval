# Run iterative predictive analysis, looping over each individual's data

Run iterative predictive analysis, looping over each individual's data

## Usage

``` r
run_eval(
  model,
  data,
  ids = NULL,
  parameters = NULL,
  fixed = NULL,
  omega = NULL,
  iov = NULL,
  ruv = NULL,
  dictionary = list(),
  group = NULL,
  weights = NULL,
  weight_prior = 1,
  censor_covariates = TRUE,
  incremental = FALSE,
  .stats_summ_options = stats_summ_options(),
  .vpc_options = vpc_options(),
  .fit_options = fit_options(),
  threads = 1,
  progress = TRUE,
  verbose = TRUE
)
```

## Arguments

- model:

  either a PKPDsim model object, or a string pointing to a
  PKPDsim-generated model library, e.g. `pkvancothomson`

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- ids:

  optional, vector of subject IDs to run analysis on (by default runs
  analysis on all subjects in dataset)

- parameters:

  list of parameters

- fixed:

  fix a specific parameters, supplied as vector of strings

- omega:

  between subject variability, supplied as vector specifiying the lower
  triangle of the covariance matrix of random effects

- iov:

  a list specifying the required metadata for implementation of IOV,
  specifically the coefficient of variation (CV %) of the IOV for each
  parameter and a vector of bin separators. For example,
  `list(cv = list(CL = 0.1, V = 0.2), bins = c(0, 24, 48, 9999))`

- ruv:

  residual error variability magnitude, specified as list.

- dictionary:

  data dictionary. Optional, a named character vector that specifies the
  column names to be used from the dataset.

- group:

  name of column in `data` that groups observations together in
  iterative flow. By default each observation will be its own "group",
  but this can be used to group peaks and troughs together, or to group
  observations on the same day together. Grouping will be done prior to
  running the analysis, so cannot be changed afterwards.

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

- .stats_summ_options:

  Options for summary statistics. This must be the result from a call to
  [`stats_summ_options()`](https://insightrx.github.io/mipdeval/dev/reference/stats_summ_options.md).

- .vpc_options:

  Options for VPC simulations. This must be the result from a call to
  [`vpc_options()`](https://insightrx.github.io/mipdeval/dev/reference/vpc_options.md).

- .fit_options:

  Options for controlling MAP Bayesian fit. This must be the result from
  a call to
  [`fit_options()`](https://insightrx.github.io/mipdeval/dev/reference/fit_options.md).

- threads:

  number of threads to divide computations on. Default is 1, i.e. no
  parallel execution

- progress:

  should a progress bar be shown? Default is `TRUE`, but when debugging
  the package it is useful to have it off, since progress bar handlers
  obscure R output.

- verbose:

  show more output

## Value

An `mipdeval_results` object, which is a named list with the following
elements:

- `results`: A tibble with one row per (iterative) prediction, holding
  the identifiers (`id`, `_iteration`, `_grouper`, `t`), the observation
  (`dv`), population predictions and residuals (`pred`, `res`, `wres`,
  `cwres`), individual predictions and residuals (`iter_ipred`,
  `map_ipred`, `ires`, `iwres`), the objective function value (`ofv`),
  the weighted sum-of-squares (`ss_w`), an `apriori` flag, one column
  per model parameter, and two families of eta (random-effect) columns:
  the iterative `eta<nn>` and the full-data `map_eta<nn>` estimates (see
  Details).

- `mod_obj`: The parsed model object (see
  [`parse_model()`](https://insightrx.github.io/mipdeval/dev/reference/parse_model.md)):
  a named list of model information, including `model`, `parameters`,
  `omega`, `ruv`, `fixed`, `bins`, and `kappa`.

- `data`: The input data after reading and validation (see
  [`read_input_data()`](https://insightrx.github.io/mipdeval/dev/reference/read_input_data.md)
  and
  [`check_input_data()`](https://insightrx.github.io/mipdeval/dev/reference/check_input_data.md)),
  as a data frame of the NONMEM-style records used in the analysis.

- `sim`: Simulated data used for the visual predictive check (VPC) and
  NPDE, or `NULL` when simulations are skipped
  (`vpc_options(skip = TRUE)`).

- `stats_summ`: A tibble summarising forecasting performance statistics
  (see
  [`calculate_stats()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_stats.md)).

- `shrinkage`: A tibble of eta-shrinkage per iteration (see
  [`calculate_shrinkage()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_shrinkage.md)).

- `bayesian_impact`: A tibble of Bayesian-impact values based on RMSE
  and MAPE (see
  [`calculate_bayesian_impact()`](https://insightrx.github.io/mipdeval/dev/reference/calculate_bayesian_impact.md)).

`stats_summ`, `shrinkage`, and `bayesian_impact` are `NULL` when no
predictions are produced (e.g. `vpc_options(vpc_only = TRUE)`).

## Details

`run_eval()` evaluates predictive performance the way a model would be
used for model-informed precision dosing (MIPD): for each subject it
walks through the observations in time order and, at each step, refits
the individual using only the data available *up to that point* to
predict the next observation(s). This produces an a priori (population)
prediction followed by progressively more informed a posteriori
(forecasting) predictions, mirroring the iterative flow of PsN's
`proseval` tool.

Because of this design the `results` tibble reports quantities at three
levels of individualization, which are easy to confuse. The predictions:

- `pred`: the **population** prediction (typical individual, using no
  individual-level data)—the a priori prediction.

- `iter_ipred`: the **iterative ("forecasting")** individual prediction,
  using only the data available up to that point—the a posteriori
  prediction. On the a priori rows it falls back to `pred`.

- `map_ipred`: the **full-data MAP** individual prediction, from a
  single retrospective fit on all of a subject's data at once.

The random-effect (eta) estimates follow the same structure:

- The `eta<nn>` columns are the iterative ("forecasting") empirical
  Bayes estimates that pair with `iter_ipred`; they evolve down the rows
  and are 0 on the a priori (population) rows.

- The `map_eta<nn>` columns are the full-data MAP empirical Bayes
  estimates that pair with `map_ipred`. They are constant per subject
  and appear on every row, including the a priori row. These are the
  equivalent of the etas reported in a NONMEM output table, and are what
  you want for an empirical eta-distribution plot (one value per
  subject).
