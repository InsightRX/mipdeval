# AGENTS.md

This file provides guidance to AI coding agents when working with code in this
repository. It is harness-agnostic: any agent should read it before making
changes.

## Package Overview

`mipdeval` is an R package for evaluating predictive performance of PK/PD models
in historical datasets for model-informed precision dosing (MIPD). It
iteratively loops through NONMEM-style datasets, performing MAP Bayesian
updating and forecasting at each step—similar to PsN's `proseval` tool, but with
MIPD-specific features like prior flattening, covariate censoring, incremental
(MPC-style) fitting, and sample grouping.

Core dependencies:
- **PKPDmap** (InsightRX/PKPDmap): MAP Bayesian fitting via `get_map_estimates()`
- **PKPDsim** (InsightRX/PKPDsim): PK/PD model objects and regimen handling

Both are GitHub-only (see `Remotes:` in `DESCRIPTION`); `pak::pak()` /
`devtools::install_dev_deps()` is needed to get them.

## Common Commands

```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file (matches tests/testthat/test-<filter>.R)
devtools::test(filter = "run_eval")

# Regenerate documentation (required after changing roxygen2 comments)
devtools::document()

# Full package check (what CI runs, via R-CMD-check.yaml)
devtools::check()

# Rebuild README.md from README.Rmd (never edit README.md directly)
devtools::build_readme()

# Build vignettes / pkgdown site
devtools::build_vignettes()
pkgdown::build_site()
```

CI (`.github/workflows/R-CMD-check.yaml`) runs `R CMD check` on ubuntu-latest /
R release. `pkgdown.yml` deploys the site; `update-citation-cff.yaml` keeps
`CITATION.cff` in sync with `DESCRIPTION`.

## Architecture

### Core Evaluation Flow

The main entry point is `run_eval()` (`R/run_eval.R`), which orchestrates:

1. **Model parsing** (`parse_model.R`): S3 dispatch (`parse_model.PKPDsim`,
   `parse_model.character`, `parse_model.default`) handles PKPDsim model
   objects, character strings naming an installed model library (e.g.
   `"pkvancothomson"`), or PKPDsim object types. Extracts parameters, omega,
   ruv, fixed effects, `kappa`/`as_eta`, and IOV bins into a single `mod_obj`
   list that is passed down the pipeline.

2. **Data reading and validation** (`read_input_data.R`, `check_input_data.R`):
   accepts a file path or a `data.frame`. Requires columns `ID`, `TIME`, `EVID`,
   `DV`; a `dictionary` argument renames non-standard columns and its names must
   be a subset of those four.

3. **Data parsing** (`parse_input_data.R`): splits the dataset by subject `ID`
   and builds per-subject regimen / observation / covariate structures in the
   shape PKPDsim and PKPDmap expect. Adds an internal `_grouper` column: either
   the user-supplied `group` column, or a per-observation counter when no
   grouping is requested. The `_grouper` defines the iterations of the core loop.

4. **Core iterative loop** (`run_eval_core.R`): called once per subject and
   parallelized through `run()`. For each iteration it:
   - picks which samples enter the fit via `handle_sample_weighting()` —
     cumulative by default (all samples up to and including the current group
     get weight 1), or only the current group when `incremental = TRUE`;
   - censors future covariates via `handle_covariate_censoring()` to prevent
     data leakage (`censor_covariates = TRUE`; set `FALSE` to reproduce
     `PsN::proseval` behaviour exactly);
   - when `incremental = TRUE`, feeds the previous iteration's MAP estimates and
     their uncertainty (`fit$vcov`) in as the new `parameters`/`omega` — the
     "model predictive control (MPC)" approach;
   - calls `PKPDmap::get_map_estimates()` and assembles a tibble of `dv`,
     `ipred`, `pred`, residuals (`ires`, `iwres`, `res`, `wres`, `cwres`),
     `ofv`, `ss_w` plus `_iteration` / `_grouper`.

   A failed fit returns an `error` object; the branch emits an all-`NA` row set
   with the *same shape* as a successful fit (including `NA` eta columns once
   eta names are known), so downstream binding never breaks.

5. **VPC / NPDE simulation** (`run_vpc.R`): `run_vpc_core()` is run over the same
   per-subject list, controlled by `vpc_options()` (`skip`, `vpc_only`,
   `n_samples`, `seed`).

6. **Statistics** (`calculate_stats.R`, `calculate_bootstrap_summ.R`,
   `bootstrap_metrics.R`, `calculate_shrinkage.R`,
   `calculate_bayesian_impact.R`): RMSE, NRMSE, MAPE, MPE, accuracy
   (`is_accurate_abs` / `is_accurate_rel`), optional bootstrap confidence
   intervals, shrinkage, and Bayesian-updating impact.

Failed fits (which surface as `NA` predictions) are detected and reported by
`check_failed_fits.R`. `run_eval()` calls it once after the per-subject loop to
emit a single warning; `calculate_stats()` also calls it (controllable via its
`warn` argument, which `run_eval()` sets to `FALSE` to avoid a duplicate
warning).

### Output Structure

`run_eval()` returns a list with class `"mipdeval_results"`, with elements in
this order (tests assert on exactly these names):

- `results`: full prediction and parameter data
- `mod_obj`: parsed model information
- `data`: input dataset (post-dictionary-renaming)
- `sim`: VPC simulations (`NULL` if skipped)
- `stats_summ`: summary statistics
- `bootstrap_summ`: bootstrapped statistics (`NULL` unless
  `bootstrap_options(skip = FALSE)`)
- `shrinkage`: shrinkage metrics
- `bayesian_impact`: Bayesian updating impact

`print()` and `plot()` methods exist for `mipdeval_results` and for the
`stats_summ` / `shrinkage` / `bayesian_impact` sub-objects
(`print_functions.R`, `plot.R`).

### Key Design Patterns

- **Configuration helpers**: `vpc_options()`, `fit_options()`,
  `stats_summ_options()`, `bootstrap_options()` return typed, classed option
  lists — use these instead of raw lists for the `.`-prefixed arguments of
  `run_eval()`. They validate with `vctrs::vec_assert()` and `rlang::check_dots_empty()`,
  so every new option must be added to the helper, not passed ad hoc. Bootstrap
  settings are nested inside `stats_summ_options(bootstrap = ...)` and share its
  accuracy error margins.
- **Sample weighting** (`handle_sample_weighting()` in `run_eval_core.R`):
  returns a 0/1 weight vector per observation. Observations with weight 0 still
  receive predictions in the output — they are simply not weighted in the fit.
- **Grouping columns** (`add_grouping_column.R`): group observations by dose or
  time range with `group_by_dose()` / `group_by_time()`, then pass the resulting
  column name as `group =`.
- **Parallelism**: `run()` (`run.R`) wraps `purrr::map()` with optional `furrr`
  parallelism (`threads > 1` sets a `future::multisession` plan) and supports
  skipping the whole step (`.skip`). Results are always combined with
  `purrr::list_rbind()`, so `.f` must return a data.frame-like object.
- **Progress**: `progressr` + `cli` handlers, because `furrr` cannot drive cli
  progress bars directly.
- **Error style**: user-facing errors use `cli::cli_abort()` with `{.arg}` /
  `{.field}` inline markup; match this style in new checks.

### Data and Reference Results

- Lazy-loaded example datasets `nm_vanco` and `nm_busulfan` live in `data/` and
  are regenerated from the scripts in `data-raw/` (documented in `R/data.R`).
- `inst/extdata/` holds raw CSVs plus reference PsN output. `vanco_thomson.csv`
  is PsN `proseval` output used to validate `run_eval()` results within ~10%
  tolerance (the Thomson model's high IIV means max deviation is ~8%).
- `compare_psn_results.R` / `parse_psn_proseval_results.R` provide
  `compare_psn_*_results()` and `reldiff_psn_*_results()` for those comparisons.

### Test Infrastructure

Tests use testthat edition 3. `tests/testthat/setup.R` installs required PKPDsim
model libraries (`pkvancothomson`, `pkvbusulfanshukla`) before tests run — the
first test run therefore needs network access. `tests/testthat/helper.R` provides
`local_mipdeval_options()`, which silences cli/rlang output; call it at the top
of tests that invoke `run_eval()`. Visual regression tests use `vdiffr` with
snapshots in `tests/testthat/_snaps`.

## General Notes

- After any change that affects project structure, file locations, or
  architecture, update this file to reflect the new state.
- **Documentation**: after any change to a function's roxygen2 description or
  signature, regenerate the package documentation with `devtools::document()`.
  The generated files in `man/` and `NAMESPACE` are checked in, but their diffs
  can be ignored during review.
- `README.md` is generated from `README.Rmd` — edit the `.Rmd`.
