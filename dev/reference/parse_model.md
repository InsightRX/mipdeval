# Parse PKPDsim model information

Parse model information from PKPDsim model object or an installed
PKPDsim model library.

## Usage

``` r
parse_model(model, ...)

# S3 method for class 'character'
parse_model(
  model,
  ...,
  parameters = NULL,
  ruv = NULL,
  omega = NULL,
  fixed = NULL,
  iov = NULL
)

# S3 method for class 'PKPDsim'
parse_model(model, ..., parameters, ruv, omega, fixed = NULL, iov = NULL)
```

## Arguments

- model:

  either a PKPDsim model object, or a string pointing to a
  PKPDsim-generated model library, e.g. `pkvancothomson`

- ...:

  Arguments passed to methods.

- parameters:

  list of parameters

- ruv:

  residual error variability magnitude, specified as list.

- omega:

  between subject variability, supplied as vector specifiying the lower
  triangle of the covariance matrix of random effects

- fixed:

  fix a specific parameters, supplied as vector of strings

- iov:

  a list specifying the required metadata for implementation of IOV,
  specifically the coefficient of variation (CV %) of the IOV for each
  parameter and a vector of bin separators. For example,
  `list(cv = list(CL = 0.1, V = 0.2), bins = c(0, 24, 48, 9999))`

## Value

A named list.
