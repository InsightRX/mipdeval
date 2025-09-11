test_that("Basic run with vanco data + model works", {
  local_mipdeval_options()

  # Using PKPDsim model object:
  mod_obj <- parse_model("pkvancothomson")
  res <- run_eval(
    model = mod_obj$model,
    data = nm_vanco,
    parameters = mod_obj$parameters,
    omega = mod_obj$omega_matrix,
    ruv = mod_obj$ruv,
    censor_covariates = FALSE, # shouldn't matter, since no time-varying covs
    progress = FALSE
  )

  ## Expected structure:
  expect_equal(names(res), c("results", "stats"))
  expect_s3_class(res$results, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(res$stats, c("tbl_df", "tbl", "data.frame"))

  # Using PKPDsim model library:
  res_2 <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    censor_covariates = FALSE, # shouldn't matter, since no time-varying covs
    progress = FALSE
  )

  ## Results should be identical between PKPDsim model object and model library
  ## runs if all inputs are the same:
  expect_identical(res, res_2)

  ## Reference results from PsN proseval:
  proseval <- parse_psn_proseval_results(read.csv(
    file = system.file(
      package = "mipdeval",
      "proseval_reference/vanco_thomson.csv"
    )
  ))

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  expect_equal(
    nrow(proseval),
    nrow(res$results |> dplyr::filter(!apriori))
  )
  ## comparable iterative predictions
  expect_true(compare_psn_proseval_results(res, proseval, tol = 0.1)$within_tol)
})

test_that("Run also works when `model` argument just references the package", {
  local_mipdeval_options()
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    parameters = list(CL = 5, V = 50, TH_CRCL = 1, Q = 3, V2 = 40),
    progress = F,
    ids = c(1:3)
  )
  expect_equal(names(res), c("results", "stats"))
  # TODO: test outputs
})

test_that("Flattening of prior results in different predictions", {
  local_mipdeval_options()
  res <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    progress = F,
    ids = c(1:3)
  )
  res_flat <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    weight_prior = 0.8,
    progress = F,
    ids = c(1:3)
  )
  expect_equal(names(res_flat), c("results", "stats"))
  expect_true(
    all(
      res$results |>
        dplyr::filter(id == 1 & iter > 0) |>
        dplyr::pull(iter_ipred) |>
        round(2) !=
      res_flat$results |>
        dplyr::filter(id == 1 & iter > 0) |>
        dplyr::pull(iter_ipred) |>
        round(2)
    )
  )
})
