# Busulfan data

Busulfan data simulated with the `"pkbusulfanshukla"` model.

## Usage

``` r
nm_busulfan
```

## Format

A NONMEM-style data frame with 240 rows and 13 columns:

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

- `AGE`:

  Patient age at TIME of measurement.

- `WT`:

  Patient weight at TIME of measurement.

- `HT`:

  Patient height at TIME of measurement.

- `SEX`:

  Patient sex (0 = female, 1 = male).

- `REGI`:

  Regimen type.

## References

Shukla et al. Front Pharmacol. 2020.
<https://doi.org/10.3389/fphar.2020.00888>
