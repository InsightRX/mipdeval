# Calculate eta-shrinkage

Calculate eta-shrinkage, measure of how much information is available to
update individual estimates away from the population value.

## Usage

``` r
calculate_shrinkage(.res)
```

## Arguments

- .res:

  output object (`mipdeval_results`) from
  [`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md),
  or `data.frame` with raw results.

## Value

tibble

## Details

Shrinkage for population PK models was first defined in this paper by
Savic and Karlsson: https://pmc.ncbi.nlm.nih.gov/articles/PMC2758126/.
It is a measure of how much information is available to update
individual estimates away from the population value. In principle, if
there is no information at all (i.e. in the case of population estimates
only), shrinkage will be 100%. In the case of fully informed Bayesian
estimates (unlikely to be achieved in practice), shrinkage is 0%. In
most practical scenarios with limited sampling, shrinkage will be
between 10-40%. When shrinkage is higher than 50% or so, one could argue
there is limited benefit of sampling at all. So the shrinkage reported
in this package can be used to evaluate whether the sampling was
efficient, and how shrinkage could be reduced with additional sampling.
