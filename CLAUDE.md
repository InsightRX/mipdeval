# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`mipdeval` is an R package for evaluating predictive performance of PK/PD models in historical datasets for model-informed precision dosing (MIPD). It iteratively loops through NONMEM-style datasets, performing MAP Bayesian updating and forecasting at each step—similar to PsN's `proseval` tool, but with MIPD-specific features like prior flattening, time-based weighting, and sample grouping.

Core dependencies:
- **PKPDmap** (InsightRX/PKPDmap): MAP Bayesian fitting via `get_map_estimates()`
- **PKPDsim** (InsightRX/PKPDsim): PK/PD model objects and regimen handling

## Common Commands

```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
devtools::test(filter = "run_eval")

# Regenerate documentation (required after changing roxygen2 comments)
devtools::document()

# Full package check
devtools::check()
```

## Architecture

### Core Evaluation Flow

The main entry point is `run_eval()` (`R/run_eval.R`), which orchestrates:

1. **Model parsing** (`parse_model.R`): S3 dispatch handles PKPDsim objects, character strings (model library names), or PKPDsim object types. Extracts parameters, omega, ruv, fixed effects, and IOV bins.

2. **Data parsing** (`parse_input_data.R`): Splits NONMEM-style data by subject ID, creates regimen/observation/covariate structures, applies grouping logic.

3. **Core iterative loop** (`run_eval_core.R`): Called per-subject (parallelized via `furrr`). For each iterative observation group: applies sample weights, censors future covariates to prevent data leakage, calls `PKPDmap::get_map_estimates()`, and generates population/individual/iterative predictions.

4. **Statistics** (`calculate_stats.R`, `calculate_shrinkage.R`, `calculate_bayesian_impact.R`): Computes RMSE, NRMSE, MAPE, MPE, accuracy, shrinkage, and Bayesian impact metrics.

### Output Structure

`run_eval()` returns a list with class `"mipdeval_results"`:
- `results`: Full prediction and parameter data
- `mod_obj`: Parsed model information
- `data`: Input dataset
- `sim`: VPC simulations (if requested)
- `stats_summ`: Summary statistics
- `shrinkage`: Shrinkage metrics
- `bayesian_impact`: Bayesian updating impact

### Key Design Patterns

- **Configuration helpers**: `vpc_options()`, `fit_options()`, `stats_summ_options()` return typed option lists—use these instead of raw lists for arguments.
- **Sample weighting** (`calculate_fit_weights.R`): Multiple schemes (weight_all, weight_last_only, weight_last_two_only, weight_gradient_linear, weight_gradient_exponential).
- **Grouping columns** (`add_grouping_column.R`): Group observations by dose or time range with `group_by_dose()` / `group_by_time()`.
- **Parallelism**: `run.R` wraps `purrr::map()` with optional `furrr` parallelism.

### Test Infrastructure

Tests use testthat edition 3. `tests/testthat/setup.R` installs required PKPDsim model libraries (`pkvancothomson`, `pkvbusulfanshukla`) before tests run. Visual regression tests use `vdiffr`. Reference PsN proseval results live in `inst/extdata/vanco_thomson.csv` and are used to validate results within ~10% tolerance.
