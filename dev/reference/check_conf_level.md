# Validate a confidence level

Internal helper asserting that a confidence level is a single number
strictly between 0 and 1.

## Usage

``` r
check_conf_level(conf_level, arg = caller_arg(conf_level), call = caller_env())
```

## Arguments

- conf_level:

  The confidence level to check.

- arg, call:

  Used for error reporting.

## Value

`conf_level`, invisibly, or throws an error.
