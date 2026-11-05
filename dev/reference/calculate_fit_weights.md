# Calculate time-based sample weights for MAP Bayesian fitting

Computes the weights a weighting scheme assigns to a set of observation
times. This is the calculation
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
applies internally at each iteration when a scheme is passed to its
`weights` argument, and is exported so the effect of a scheme and its
parameters can be previewed before running an evaluation. To use a
scheme in
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
pass a
[`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
object, not the output of this function.

## Usage

``` r
calculate_fit_weights(weights = NULL, t = NULL)
```

## Arguments

- weights:

  weighting scheme: a
  [`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
  object, a string with the scheme name, or a named list with a `scheme`
  element plus optional scheme-specific parameters.

- t:

  numeric vector of observation times (in hours)

## Value

numeric vector of weights the same length as `t`, or `NULL` if `weights`
is `NULL` or the scheme is not recognized.

## Details

`weights` may be a
[`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
object, a string naming a scheme, or a named list with a `scheme`
element plus optional scheme-specific parameters (e.g.
`list(scheme = "weight_gradient_exponential", t12_decay = 72)`). See
[`fit_weights()`](https://insightrx.github.io/mipdeval/dev/reference/fit_weights.md)
for the available schemes and their parameters.

## Examples

``` r
t <- c(0, 24, 48, 96)
calculate_fit_weights(fit_weights("weight_last_two_only"), t = t)
#> [1] 0 0 1 1
calculate_fit_weights(
  fit_weights("weight_gradient_exponential", t12_decay = 72),
  t = t
)
#> [1] 0.3968503 0.5000000 0.6299605 1.0000000
```
