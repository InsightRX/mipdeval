require("pkvancothomson")
test_that("Basic run with vanco data + model works", {
  mod <- pkvancothomson::model()
  nm_vanco <- read.csv(file = system.file(package = "mipdeval", "data/nm_vanco.csv"))
  res <- run_eval(
    model = mod,
    data = nm_vanco,
    parameters = pkvancothomson::parameters(),
    omega = pkvancothomson::omega_matrix(),
    ruv = pkvancothomson::ruv(),
    progress = FALSE
  )
  expect_equal(names(res), c("results", "stats"))
})
