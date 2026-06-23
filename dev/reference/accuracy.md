# Accuracy

Accuracy provides a measure of clinical suitability, defined by whether
model predicted drug concentrations fall within an absolute OR relative
error margin of the measured concentrations.

## Usage

``` r
accuracy(obs, pred, error_abs = 0, error_rel = 0)

is_accurate(obs, pred, error_abs = 0, error_rel = 0)

is_accurate_abs(obs, pred, error_abs = 0)

is_accurate_rel(obs, pred, error_rel = 0)
```

## Arguments

- obs:

  Observations vector.

- pred:

  Predictions vector.

- error_abs, error_rel:

  Positive number providing an absolute or relative error margin. The
  cutoff is exclusive of the error margin. Defaults to `0`, meaning no
  predictions fall within the error margin.

## Value

For `is_accurate()`, `is_accurate_abs()`, and `is_accurate_rel()`: A
logical vector indicating whether or not each predicted drug
concentration was considered accurate according to the specified
absolute or relative error margin(s).

For `accuracy()`: A single value between 0 and 1 indicating the
proportion of predicted drug concentrations that fell within the
specified absolute and relative error margins.

## Examples

``` r
# Does the predicted drug concentration fall within 0.5 mg/L error margin?
is_accurate_abs(6, 5, error_abs = 0.5)
#> [1] FALSE

# Does the predicted drug concentration fall within 25% error margin?
is_accurate_rel(6, 5, error_rel = 0.25)
#> [1] TRUE

# Does the predicted drug concentration fall within 0.5 mg/L OR 25%?
is_accurate(6, 5, error_abs = 0.5, error_rel = 0.25)
#> [1] TRUE

# What proportion of predicted drug concentrations fell within 0.5 mg/L OR 25%?
accuracy(rnorm(10, 6), rnorm(10, 5), error_abs = 0.5, error_rel = 0.25)
#> [1] 0.5
```
