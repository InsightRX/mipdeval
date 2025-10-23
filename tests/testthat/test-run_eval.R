test_that("Basic run with vanco data + model works", {
  local_mipdeval_options()

  # Using PKPDsim model object:
  mod_obj <- parse_model("pkvancothomson")
  res <- run_eval(
    model = mod_obj$model,
    data = nm_vanco,
    parameters = mod_obj$parameters,
    omega = mod_obj$omega,
    ruv = mod_obj$ruv,
    fixed = mod_obj$fixed,
    censor_covariates = FALSE, # shouldn't matter, since no time-varying covs
    .stats_summ_options = stats_summ_options(acc_error_abs = 0.5, acc_error_rel = 0.25),
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE
  )

  ## Expected structure:
  expect_equal(names(res), c("results", "mod_obj", "data", "sim", "stats_summ", "shrinkage", "bayesian_impact"))
  expect_s3_class(res$results, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(res$stats_summ, c("tbl_df", "tbl", "data.frame"))
  expect_equal(
    names(res$results),
    c("id", "_iteration", "_grouper", "t", "dv", "pred", "map_ipred",
      "ofv", "ss_w", "iter_ipred", "apriori", "CL", "V", "TH_CRCL",
      "Q", "V2")
  )
  expect_equal(
    round(res$results$CL[1:5], 3),
    c(2.99, 2.685, 2.462, 2.430, 2.439)
  )
  expect_equal(
    round(res$results$ofv[1:5], 3),
    c(NA, 6.196, 11.846, 17.046, 21.563)
  )
  expect_equal(
    round(res$results$ss_w[1:5], 3),
    c(NA, 16.839, 11.241, 27.358, 30.944)
  )

  # Using PKPDsim model library:
  res_2 <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    censor_covariates = FALSE, # shouldn't matter, since no time-varying covs
    .stats_summ_options = stats_summ_options(acc_error_abs = 0.5, acc_error_rel = 0.25),
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE
  )

  ## Results should be identical between PKPDsim model object and model library
  ## runs if all inputs are the same:
  expect_identical(res, res_2)

  ## Reference results from PsN proseval:
  proseval <- parse_psn_proseval_results(read.csv(
    file = system.file(
      package = "mipdeval",
      "extdata/vanco_thomson.csv"
    )
  ))

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  expect_equal(
    nrow(proseval),
    nrow(res$results |> dplyr::filter(!apriori))
  )
  ## comparable iterative predictions
  ## Note: difference with PsN is fairly high (max 8%), but this is due to the
  ##   Thomson model having very high IIV on some parameters
  expect_true(compare_psn_proseval_results(res, proseval, tol = 0.1)$within_tol)
})

test_that("Run also works when `model` argument just references the package", {
  local_mipdeval_options()
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    parameters = list(CL = 5, V = 50, TH_CRCL = 1, Q = 3, V2 = 40),
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE,
    ids = c(1:3)
  )
  expect_equal(names(res), c("results", "mod_obj", "data", "sim", "stats_summ", "shrinkage", "bayesian_impact"))
  # TODO: test outputs
})

test_that("Flattening of prior results in different predictions", {
  local_mipdeval_options()
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE,
    ids = c(1:3)
  )
  res_flat <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    weight_prior = 0.8,
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE,
    ids = c(1:3)
  )
  expect_equal(names(res), c("results", "mod_obj", "data", "sim", "stats_summ", "shrinkage", "bayesian_impact"))
  expect_true(
    all(
      res$results |>
        dplyr::filter(id == 1 & `_iteration` > 0) |>
        dplyr::pull(iter_ipred) |>
        round(2) !=
      res_flat$results |>
        dplyr::filter(id == 1 & `_iteration` > 0) |>
        dplyr::pull(iter_ipred) |>
        round(2)
    )
  )
})

test_that("Run also works when `dictionary` is used", {
  local_mipdeval_options()
  nm_vanco <- dplyr::rename(nm_vanco, id2 = "ID", time2 = "TIME", evid2 = "EVID")
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    dictionary = c(ID = "id2", TIME = "time2", EVID = "evid2"),
    .vpc_options = vpc_options(skip = TRUE),
    progress = FALSE,
    ids = 1
  )
  expect_equal(names(res), c("results", "mod_obj", "data", "sim", "stats_summ", "shrinkage", "bayesian_impact"))
})

test_that("Incremental Bayes method works", {
  local_mipdeval_options()
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    incremental = TRUE,
    progress = F,
    ids = c(1:3)
  )
  expect_equal(names(res), c("results", "mod_obj", "data", "sim", "stats_summ", "shrinkage", "bayesian_impact"))
  expect_equal(
    res$stats_summ,
    structure(list(
      type = c("iter_ipred", "iter_ipred", "map_ipred",
"map_ipred", "pred", "pred"),
      apriori = c(0, 1, 0, 1, 0, 1),
      rmse = c(4.051, 8.299, 1.96, 4.319, 4.789, 8.299),
      nrmse = c(0.411, 0.232, 0.199, 0.121, 0.486, 0.232),
      mpe = c(-0.339, -0.004, -0.045, -0.002, -0.415, -0.004),
      mape = c(0.445, 0.246, 0.166, 0.118, 0.558, 0.246),
      accuracy = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
    ),
    class = c("mipdeval_results_stats_summ","tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -6L))
  )
  expect_equal(
    round(res$results$CL[1:5], 3),
    c(2.99, 2.685, 2.529, 2.668, 2.666) # should be different CL progression (expect first 2 values) than from test above (non-incremental)
  )
})
