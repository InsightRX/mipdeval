require("pkvancothomson")
library(dplyr)

test_that("Basic run with vanco data + model works", {
  mod <- pkvancothomson::model()
  res <- run_eval(
    model = mod,
    data = nm_vanco,
    parameters = pkvancothomson::parameters(),
    omega = pkvancothomson::omega_matrix(),
    ruv = pkvancothomson::ruv(),
    censor_covariates = FALSE, # shouldn't matter, since no time-varying covs
    progress = FALSE
  )
  expect_equal(names(res), c("results", "stats"))

  ## Check that it matches proseval results in NONMEM / PsN
  proseval <- read.csv(
    file = system.file(
      package = "mipdeval",
      "proseval_reference/vanco_thomson.csv"
    )
  ) |>
    parse_psn_proseval_results()
  ## Same number of rows
  expect_equal(
    nrow(proseval),
    nrow(res$results |> filter(!apriori))
  )
  ## comparable iterative predictions
  comp <- res$results |>
    filter(!apriori) |>
    mutate(proseval = proseval$pred) |>
    mutate(
      df = iter_ipred - proseval,
      rel_diff = df / proseval
    ) |>
    arrange(rel_diff)
  expect_true(all(abs(comp$rel_diff) < 0.10))
})

test_that("Run also works when `model` argument just references the package", {
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    parameters = list(CL = 5, V = 50, TH_CRCL = 1, Q = 3, V2 = 40),
    progress = F
  )
  expect_equal(names(res), c("results", "stats"))
})
