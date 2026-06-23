# mipdeval

The goal of **mipdeval** is to make it easy to evaluate predictive
performance of PK/PD models in historical datasets in the context of
model-informed precision dosing (MIPD).

``` r

library(mipdeval)
```

## Evaluate iterative predictive performance of an installed model

[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
is a nice way to evaluate iterative predictive performance of a
**PKPDsim** model using a NONMEM-style dataset, performing MAP Bayesian
updating and forecasting at each step.
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
can evaluate both in-memory PKPDsim model objects and installed PKPDsim
model libraries (see
[`vignette("PKPDsim", package = "PKPDsim")`](https://insightrx.github.io/PKPDsim/articles/PKPDsim.html)
for an overview of these different model formats).

Here we will evaluate the predictive performance of the
`"pkvancothomson"` model, which is packaged within PKPDsim and can be
installed as a model library with:

``` r

install_default_literature_model("pk_vanco_thomson", force = TRUE)
```

For this example we use the `nm_vanco` data set, a NONMEM-style data
frame with 80 rows and 11 columns:

- `ID`: Patient ID.
- `TIME`: The recorded times of dosing events and/or observations
- `CMT`: Dose compartment.
- `DV`: The dependent variable or observed concentration.
- `AMT`: Dose administered for dosing record.
- `EVID`: Event ID (0 = observation, 1 = dose, 2 = other, 3 = reset, 4 =
  reset + dose).
- `MDV`: Missing DV (0 = not missing, 1 = missing).
- `RATE`: Rate of drug infusion.
- `WT`: Patient weight at TIME of measurement.
- `CRCL`: Patient creatinine clearance at TIME of measurement.
- `CL_HEMO`: Additional clearance attributable to hemodialysis.

``` r

nm_vanco
#>    ID TIME CMT   DV  AMT EVID MDV RATE        WT     CRCL CL_HEMO
#> 1   1    0   1  0.0 1500    1   1  750  82.70184 3.687141       0
#> 2   1   12   1  0.0 1500    1   1  750  82.70184 3.687141       0
#> 3   1   15   1 38.9    0    0   0    0  82.70184 3.687141       0
#> 4   1   23   1 26.9    0    0   0    0  82.70184 3.687141       0
#> 5   1   24   1  0.0 1500    1   1  750  82.70184 3.687141       0
#> 6   1   50   1 12.1    0    0   0    0  82.70184 3.687141       0
#> 7   1   55   1  7.5    0    0   0    0  82.70184 3.687141       0
#> 8   1   71   1  5.4    0    0   0    0  82.70184 3.687141       0
#> 9   2    0   1  0.0 1500    1   1  750  82.70184 3.687141       0
#> 10  2   12   1  0.0 1500    1   1  750  82.70184 3.687141       0
#> 11  2   15   1 26.5    0    0   0    0  66.24008 3.601273       0
#> 12  2   23   1 13.9    0    0   0    0  66.24008 3.601273       0
#> 13  2   24   1  0.0 1500    1   1  750  66.24008 3.601273       0
#> 14  2   50   1 13.4    0    0   0    0  66.24008 3.601273       0
#> 15  2   55   1  8.4    0    0   0    0  66.24008 3.601273       0
#> 16  2   71   1  4.7    0    0   0    0  66.24008 3.601273       0
#> 17  3    0   1  0.0 1500    1   1  750  66.24008 3.601273       0
#> 18  3   12   1  0.0 1500    1   1  750  66.24008 3.601273       0
#> 19  3   15   1 41.8    0    0   0    0  65.20231 4.450120       0
#> 20  3   23   1 14.7    0    0   0    0  65.20231 4.450120       0
#> 21  3   24   1  0.0 1500    1   1  750  65.20231 4.450120       0
#> 22  3   50   1  3.1    0    0   0    0  65.20231 4.450120       0
#> 23  3   55   1  5.1    0    0   0    0  65.20231 4.450120       0
#> 24  3   71   1  3.0    0    0   0    0  65.20231 4.450120       0
#> 25  4    0   1  0.0 1500    1   1  750  65.20231 4.450120       0
#> 26  4   12   1  0.0 1500    1   1  750  65.20231 4.450120       0
#> 27  4   15   1 37.2    0    0   0    0  71.23560 5.691443       0
#> 28  4   23   1 16.4    0    0   0    0  71.23560 5.691443       0
#> 29  4   24   1  0.0 1500    1   1  750  71.23560 5.691443       0
#> 30  4   50   1 14.6    0    0   0    0  71.23560 5.691443       0
#> 31  4   55   1  7.9    0    0   0    0  71.23560 5.691443       0
#> 32  4   71   1  6.4    0    0   0    0  71.23560 5.691443       0
#> 33  5    0   1  0.0 1500    1   1  750  71.23560 5.691443       0
#> 34  5   12   1  0.0 1500    1   1  750  71.23560 5.691443       0
#> 35  5   15   1 23.6    0    0   0    0 109.83139 5.555126       0
#> 36  5   23   1  9.4    0    0   0    0 109.83139 5.555126       0
#> 37  5   24   1  0.0 1500    1   1  750 109.83139 5.555126       0
#> 38  5   50   1  8.5    0    0   0    0 109.83139 5.555126       0
#> 39  5   55   1  9.9    0    0   0    0 109.83139 5.555126       0
#> 40  5   71   1  2.7    0    0   0    0 109.83139 5.555126       0
#> 41  6    0   1  0.0 1500    1   1  750 109.83139 5.555126       0
#> 42  6   12   1  0.0 1500    1   1  750 109.83139 5.555126       0
#> 43  6   15   1 36.3    0    0   0    0  82.71387 4.552145       0
#> 44  6   23   1 21.3    0    0   0    0  82.71387 4.552145       0
#> 45  6   24   1  0.0 1500    1   1  750  82.71387 4.552145       0
#> 46  6   50   1 13.3    0    0   0    0  82.71387 4.552145       0
#> 47  6   55   1  8.8    0    0   0    0  82.71387 4.552145       0
#> 48  6   71   1  3.4    0    0   0    0  82.71387 4.552145       0
#> 49  7    0   1  0.0 1500    1   1  750  82.71387 4.552145       0
#> 50  7   12   1  0.0 1500    1   1  750  82.71387 4.552145       0
#> 51  7   15   1 35.6    0    0   0    0  91.01081 3.884790       0
#> 52  7   23   1 23.7    0    0   0    0  91.01081 3.884790       0
#> 53  7   24   1  0.0 1500    1   1  750  91.01081 3.884790       0
#> 54  7   50   1 13.8    0    0   0    0  91.01081 3.884790       0
#> 55  7   55   1 12.8    0    0   0    0  91.01081 3.884790       0
#> 56  7   71   1 11.5    0    0   0    0  91.01081 3.884790       0
#> 57  8    0   1  0.0 1500    1   1  750  91.01081 3.884790       0
#> 58  8   12   1  0.0 1500    1   1  750  91.01081 3.884790       0
#> 59  8   15   1 23.8    0    0   0    0 115.47157 5.420473       0
#> 60  8   23   1 11.6    0    0   0    0 115.47157 5.420473       0
#> 61  8   24   1  0.0 1500    1   1  750 115.47157 5.420473       0
#> 62  8   50   1  5.3    0    0   0    0 115.47157 5.420473       0
#> 63  8   55   1  5.5    0    0   0    0 115.47157 5.420473       0
#> 64  8   71   1  4.7    0    0   0    0 115.47157 5.420473       0
#> 65  9    0   1  0.0 1500    1   1  750 115.47157 5.420473       0
#> 66  9   12   1  0.0 1500    1   1  750 115.47157 5.420473       0
#> 67  9   15   1 16.7    0    0   0    0  88.42384 6.282196       0
#> 68  9   23   1  9.5    0    0   0    0  88.42384 6.282196       0
#> 69  9   24   1  0.0 1500    1   1  750  88.42384 6.282196       0
#> 70  9   50   1  2.2    0    0   0    0  88.42384 6.282196       0
#> 71  9   55   1  3.2    0    0   0    0  88.42384 6.282196       0
#> 72  9   71   1  1.2    0    0   0    0  88.42384 6.282196       0
#> 73 10    0   1  0.0 1500    1   1  750  88.42384 6.282196       0
#> 74 10   12   1  0.0 1500    1   1  750  88.42384 6.282196       0
#> 75 10   15   1 51.6    0    0   0    0  64.28087 5.032994       0
#> 76 10   23   1 17.3    0    0   0    0  64.28087 5.032994       0
#> 77 10   24   1  0.0 1500    1   1  750  64.28087 5.032994       0
#> 78 10   50   1 13.0    0    0   0    0  64.28087 5.032994       0
#> 79 10   55   1  8.6    0    0   0    0  64.28087 5.032994       0
#> 80 10   71   1  7.4    0    0   0    0  64.28087 5.032994       0
```

The predictive performance of the model can then be evaluated using
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md).
When using an installed PKPDsim model library, the `model` argument
simply requires the name of the library.

``` r

run_eval(model = "pkvancothomson", data = nm_vanco)
```

## Evaluate iterative predictive performance of an in-memory model

When evaluating in-memory PKPDsim model objects,
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
requires several additional arguments in addition to the model object:

- `parameters`: Named list of model parameters.
- `ruv`: Named list specifying proportional and/or additive residual
  error variability magnitude.
- `omega`: Between-subject variability, supplied as vector specifying
  the lower triangle of the covariance matrix of random effects.

Here we will replicate the model from the previous example by hand:

``` r

mod <- new_ode_model(
  code = ("
    dAdt[0] = -(CLtot/Vi)*A[0] - (Q/Vi)*A[0] + (Q/V2i)*A[1]
    dAdt[1] = +(Q/Vi)*A[0] - (Q/V2i)*A[1]
    dAdt[2] = A[0]/Vi
  "),
  pk_code = ("
    CLi = CL * (1 + TH_CRCL * (CRCL*16.66667 - 66))
    CLtot = CLi + CL_HEMO
    Qi = Q
    Vi = V * WT
    V2i = V2 * WT
  "),
  covariates = c("WT", "CRCL", "CL_HEMO"),
  declare_variables = c("CLi", "Qi", "Vi", "V2i", "CLtot"),
  fixed = "TH_CRCL",
  obs = list(scale = "V * WT")
)
parameters <- list(CL = 2.99, V = 0.675, TH_CRCL = 0.0154, Q = 2.28, V2 = 0.732)
ruv <- list(prop = 0.15, add = 1.6)
omega <- c(0.0729, 0.0100, 0.0225, 0, 0, 0.2401, 0, 0, 0, 1.6900)
```

The predictive performance of the model can then be evaluated using
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md):

``` r

run_eval(
  model = mod,
  data = nm_vanco,
  parameters = parameters,
  omega = omega,
  ruv = ruv
)
```

## Comparison with Perl-speaks-NONMEM

[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
performs similarly to the `execute` and `proseval` tools in
[Perl-speaks-NONMEM](https://uupharmacometrics.github.io/PsN/) (PsN),
while offering the following advantages:

- **Ease of use:** Evaluates a priori and a posteriori predictive
  performance in one call.
- **Tailored for MIPD:** More MIPD-focused analysis options such as
  flattening priors, time-based weighting, sample grouping, etc.

Our goal is to provide comparable (iterative) predictions between
mipdeval and PsN. The `compare_psn_*_results()` family of functions can
be used to calculate the relative difference in predictions between
[`run_eval()`](https://insightrx.github.io/mipdeval/dev/reference/run_eval.md)
and the PsN `execute` and `proseval` tools.

To compare `execute` results, use
[`compare_psn_execute_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md):

``` r

# TODO: create function
compare_psn_execute_results(run_eval_results, execute_results, tol = 0.1)
```

To compare `proseval` results, use
[`compare_psn_proseval_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md):

``` r

# TODO: create function
compare_psn_proseval_results(run_eval_results, proseval_results)
```

The underlying relative difference for each (iterative) prediction can
also be returned with
[`reldiff_psn_execute_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md)
and
[`reldiff_psn_proseval_results()`](https://insightrx.github.io/mipdeval/dev/reference/compare-psn-results.md).
