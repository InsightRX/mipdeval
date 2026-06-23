# Core function for creating visual predictive checks (VPCs). Runs `n_samples` simulations for a single subject

Core function for creating visual predictive checks (VPCs). Runs
`n_samples` simulations for a single subject

## Usage

``` r
run_vpc_core(data, mod_obj, progress_function, n_samples = 250, seed = 123)
```

## Arguments

- data:

  NONMEM-style data.frame, or path to CSV file with NONMEM data

- mod_obj:

  list object with model information

- progress_function:

  function to increment progress bar

- n_samples:

  Number of iterations to perform for VPC simulations.

- seed:

  Seed for random number generation.

## Value

data.frame
