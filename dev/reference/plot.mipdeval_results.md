# Plot method for a `run_eval()` object

One plot `type` is currently available:

- `type = "vpc"` plots visual predictive checks (VPC) with
  [`vpc::vpc()`](https://rdrr.io/pkg/vpc/man/vpc.html).

## Usage

``` r
# S3 method for class 'mipdeval_results'
plot(x, type = "vpc", ...)
```

## Arguments

- x:

  An object.

- type:

  Character string, indicating the type of plot. Options are `"vpc"`.

- ...:

  Arguments passed to or from other methods.

## Value

A plot.
