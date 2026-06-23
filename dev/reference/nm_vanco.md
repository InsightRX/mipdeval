# Vancomycin data

Vancomycin data simulated with the `"pkvancothomson"` model.

## Usage

``` r
nm_vanco
```

## Format

A NONMEM-style data frame with 80 rows and 11 columns:

- `ID`:

  Patient ID.

- `TIME`:

  The recorded times of dosing events and/or observations.

- `CMT`:

  Dose compartment.

- `DV`:

  The dependent variable or observed concentration.

- `AMT`:

  Dose administered for dosing record.

- `EVID`:

  Event ID (0 = observation, 1 = dose, 2 = other, 3 = reset, 4 = reset +
  dose).

- `MDV`:

  Missing DV (0 = not missing, 1 = missing).

- `RATE`:

  Rate of drug infusion.

- `WT`:

  Patient weight at TIME of measurement.

- `CRCL`:

  Patient creatinine clearance at TIME of measurement.

- `CL_HEMO`:

  Additional clearance attributable to hemodialysis.

## References

Thomson et al. J Antimicrob Chemotherap. 2009.
<https://doi.org/10.1093/jac/dkp085>
