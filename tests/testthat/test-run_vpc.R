test_that("Basic vpc with vanco data + model works", {
  local_mipdeval_options()

  vpc_vanc <- run_vpc(
    model = "pkvancothomson",
    data = nm_vanco,
    n_samples = 100,
    progress = FALSE
  )
  expect_equal(nrow(vpc_vanc$sim), 5000)
  expect_equal(nrow(vpc_vanc$obs), 50)
  expect_equal(names(vpc_vanc$sim), c("ID", "TIME", "DV", "ITER"))
  expect_false(any(is.na(vpc_vanc$sim$DV)))
  # vpc_obj <- vpc::vpc(
  #   sim = vpc_data$sim,
  #   obs = vpc_data$obs,
  #   show = list(obs_dv = TRUE)
  # )
})

test_that("Busulfan vpc with vanco data + model works", {
  local_mipdeval_options()

  ## use specific parameters, not model lib
  mod_obj <- parse_model("pkbusulfanshukla")
  vpc_bu <- run_vpc(
    model = "pkbusulfanshukla",
    parameters = mod_obj$parameters,
    data = nm_busulfan,
    n_samples = 100,
    progress = FALSE
  )
  expect_equal(nrow(vpc_bu$sim), 20000)
  expect_equal(nrow(vpc_bu$obs), 200)
  expect_equal(names(vpc_bu$sim), c("ID", "TIME", "DV", "ITER"))
  expect_false(any(is.na(vpc_bu$sim$DV)))
  # vpc::vpc(
  #   sim = vpc_bu$sim,
  #   obs = vpc_bu$obs,
  #   smooth = T,
  #   bins = c(unique(nm_busulfan$TIME)-0.01, 96),
  #   show = list(obs_dv = TRUE)
  # ) +
  #   ggplot2::scale_y_log10()
})
