
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mipdtrial

<!-- badges: start -->

<!-- badges: end -->

The goal of mipdeval is to make it easy to evaluate predictive
performance of PK/PD models in historical datasets, in the context of
model-informed precision dosing (specifically Bayesian updating)

## Installation

Install the development version from
[GitHub](https://github.com/InsightRX/mipdeval) with:

``` r
# install.packages("pak")
pak::pak("InsightRX/mipdeval")
```

## Usage

    run_eval(
      model,
      data
    )

<!--
## Documentation
&#10;See at <pkgdown URL> and also in the installed package: `help(package = "mipdeval")`.
-->

## Roadmap

The `mipdeval` package is currently under development, and there will
likely be changes to core functionality in the upcoming months. The
following features are on our short-term roadmap:

- match functionality in PsN::proseval
- allow timebased weighting of historical data
- allow flattening of priors
- allow adjustment of residual error magnitude
- allow grouping of samples (e.g. peak/trough, or day-to-day
  forecasting)

## Contributing

We welcome input from the community:

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/InsightRX/mipdeval/issues) on the GitHub
  page. Please include a reproducible example of the unexpected
  behavior.

- Please [open a pull
  request](https://github.com/InsightRX/mipdeval/pulls) if you have a
  fix or updates that would improve the package. If you’re not sure if
  your proposedchanges are useful or within scope of the package, feel
  free to contact one of the authors of this package.

## Disclaimer

The functionality in this R package is provided “as is”. While its
authors adhere to software development best practices, the software may
still contain unintended errors.

InsightRX Inc. and the authors of this package can not be held liable
for any damages resulting from any use of this software. By the use of
this software package, the user waives all warranties, expressed or
implied, including any warranties to the accuracy, quality or
suitability of InsightRX for any particular purpose, either medical or
non-medical.

------------------------------------------------------------------------

<div align="right">

©
<img src="man/figures/insightrx_logo_color.png" alt="InsightRX logo" width="120" />

</div>
