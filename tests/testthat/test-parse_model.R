# parse_model() ---------------------------------------------------------------
test_that("parse_model() works for PKPDsim model objects", {
  mod_obj <- withr::with_package("pkvancothomson", quietly = TRUE, {
    list(
      model = model(),
      parameters = parameters(),
      ruv = ruv(),
      omega = omega_matrix(),
      fixed = fixed(),
      bins = numeric(0),
      kappa = NULL
    )
  })
  out <- parse_model(
    model = mod_obj$model,
    parameters = mod_obj$parameters,
    ruv = mod_obj$ruv,
    omega = mod_obj$omega,
    fixed = mod_obj$fixed
  )
  expect_equal(out, mod_obj)
})

test_that("parse_model() works for installed PKPDsim model libraries", {
  mod_obj <- withr::with_package("pkvancothomson", quietly = TRUE, {
    list(
      model = model(),
      parameters = parameters(),
      ruv = ruv(),
      omega = omega_matrix(),
      fixed = fixed(),
      bins = numeric(0),
      kappa = NULL
    )
  })
  out <- parse_model("pkvancothomson")
  expect_equal(out, mod_obj)
})

test_that("parse_model() supports overrides for installed PKPDsim model libraries", {
  parameters <- list(CL = 5, V = 50, TH_CRCL = 1, Q = 3, V2 = 40)
  ruv <- list(prop = 2, add = 1)
  omega <- c(0.07, 0.0090, 0.02, 0, 0, 0.2, 0, 0, 0, 1.6)
  iov_bins = list(n_bins = 2)
  mod_obj <- withr::with_package("pkvancothomson", quietly = TRUE, {
    list(
      model = model(),
      parameters = parameters,
      ruv = ruv,
      omega = omega,
      fixed = fixed(),
      bins = numeric(0),
      kappa = NULL
    )
  })
  out <- parse_model(
    "pkvancothomson",
    parameters = parameters,
    ruv = ruv,
    omega = omega
  )
  expect_equal(out, mod_obj)
})

test_that("parse_model() output is identical across methods given same inputs", {
  mod_obj <- withr::with_package("pkvancothomson", quietly = TRUE, {
    list(
      model = model(),
      parameters = parameters(),
      ruv = ruv(),
      omega_matrix = omega_matrix(),
      kappa = NULL
    )
  })
  out_model_object <- parse_model(
    model = mod_obj$model,
    parameters = mod_obj$parameters,
    ruv = mod_obj$ruv,
    omega = mod_obj$omega_matrix,
    fixed = attr(mod_obj$model, "fixed")
  )
  out_model_library <- parse_model("pkvancothomson")
  expect_identical(out_model_object, out_model_library)
})

test_that("parse_model() output is identical across methods given same inputs for model with IOV", {
  mod_obj <- withr::with_package("pkbusulfanshukla", quietly = TRUE, {
    list(
      model = model(),
      parameters = parameters(),
      ruv = ruv(),
      omega_matrix = omega_matrix(),
      iov = iov()
    )
  })
  out_model_object <- parse_model(
    model = mod_obj$model,
    parameters = mod_obj$parameters,
    ruv = mod_obj$ruv,
    omega = mod_obj$omega_matrix,
    fixed = attr(mod_obj$model, "fixed"),
    iov = mod_obj$iov
  )
  out_model_library <- parse_model("pkbusulfanshukla")
  expect_identical(out_model_object, out_model_library)
})

test_that("parse_model() returns error for unsupported classes", {
  expect_error(
    parse_model(2),
    "`model` must be a PKPDsim model object or a character string"
  )
  expect_error(
    parse_model(list(x = 1, y = 2)),
    "`model` must be a PKPDsim model object or a character string"
  )
})

# check_installed_model_library() ---------------------------------------------
test_that("check_installed_model_library() returns NULL if model is installed", {
  expect_null(check_installed_model_library("pkvancothomson"))
})

test_that("check_installed_model_library() returns error if model not installed", {
  expect_error(
    check_installed_model_library("pkvanconotinstalled"),
    'The package "pkvanconotinstalled" is required.'
  )
})

