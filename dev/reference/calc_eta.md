# Calculate the "eta"-value for a parameters, assuming an exponential shape for IIV, i.e. `PAR = TV_PAR * EXP(ETA(n))`, and additive shape for IOV (kappa's), i.e. `PAR = TV_PAR + ETA(n)` or when final parameter estimate is 0.

Calculate the "eta"-value for a parameters, assuming an exponential
shape for IIV, i.e. `PAR = TV_PAR * EXP(ETA(n))`, and additive shape for
IOV (kappa's), i.e. `PAR = TV_PAR + ETA(n)` or when final parameter
estimate is 0.

## Usage

``` r
calc_eta(ind, par_name, parameters)
```

## Arguments

- ind:

  individual parameter estimate

- par_name:

  name of parameter in model

- parameters:

  list of parameter values, should include entry for `par_name`
