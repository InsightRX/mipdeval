# Construct a fit-weighting scheme specification

Self-documenting helper that builds a `fit_weights` object describing
how older observations should be downweighted relative to more recent
ones during the iterative MAP Bayesian fitting step. The result can be
passed as the `weights` argument to
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md).

## Usage

``` r
fit_weights(
  scheme = c("weight_all", "weight_last_only", "weight_last_two_only",
    "weight_gradient_linear", "weight_gradient_exponential"),
  ...
)
```

## Arguments

- scheme:

  name of the weighting scheme (see Details).

- ...:

  scheme-specific parameters, e.g. `t12_decay = 72` for
  `"weight_gradient_exponential"`, or
  `gradient = list(t1 = 5, w1 = 0.1, t2 = 1, w2 = 1)` for
  `"weight_gradient_linear"`.

## Value

an object of class `fit_weights`.

## Details

Available schemes:

- `"weight_all"`: all samples weighted equally (weight = 1).

- `"weight_last_only"`: only the most recent sample is used (weight =
  1), all others are excluded (weight = 0).

- `"weight_last_two_only"`: only the two most recent samples are used.

- `"weight_gradient_linear"`: weights increase linearly from a minimum
  (`w1`) for samples older than `t1` days to a maximum (`w2`) for
  samples more recent than `t2` days. Accepts scheme parameter
  `gradient`, a list with named elements `t1`, `w1`, `t2`, `w2`.
  Default: `list(t1 = 7, w1 = 0, t2 = 2, w2 = 1)`.

- `"weight_gradient_exponential"`: weights decay exponentially with the
  age of the sample. Accepts scheme parameters `t12_decay` (half-life of
  decay in hours, default 48) and `t_start` (delay in hours before decay
  starts, default 0).

## Examples

``` r
fit_weights("weight_all")
#> $scheme
#> [1] "weight_all"
#> 
#> $params
#> list()
#> 
#> attr(,"class")
#> [1] "fit_weights"
fit_weights("weight_gradient_exponential", t12_decay = 72)
#> $scheme
#> [1] "weight_gradient_exponential"
#> 
#> $params
#> $params$t12_decay
#> [1] 72
#> 
#> 
#> attr(,"class")
#> [1] "fit_weights"
fit_weights("weight_gradient_linear", gradient = list(t1 = 5, w1 = 0.1, t2 = 1, w2 = 1))
#> $scheme
#> [1] "weight_gradient_linear"
#> 
#> $params
#> $params$gradient
#> $params$gradient$t1
#> [1] 5
#> 
#> $params$gradient$w1
#> [1] 0.1
#> 
#> $params$gradient$t2
#> [1] 1
#> 
#> $params$gradient$w2
#> [1] 1
#> 
#> 
#> 
#> attr(,"class")
#> [1] "fit_weights"
```
