test_that("Basic run with vanco data + model works", {
  mod <- pkvancothomson::model()
  res <- run_eval(
    model = mod,
    data = nm_vanco,
    parameters = pkvancothomson::parameters(),
    omega = pkvancothomson::omega_matrix(),
    ruv = pkvancothomson::ruv()
  )
  expect_equal(names(res), c("results", "parameters", "stats"))
})
