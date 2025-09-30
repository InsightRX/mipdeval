test_that("Basic vpc with vanco data + model works", {
  local_mipdeval_options()

  vpc_vanc <- run_eval(
    model = "pkvancothomson",
    data = nm_vanco,
    .vpc_options = vpc_options(n_samples = 100, vpc_only = TRUE),
    progress = FALSE
  )

  # Simulation:
  expect_equal(nrow(vpc_vanc$sim), 5000)
  expect_equal(nrow(dplyr::filter(vpc_vanc$data, EVID == 0)), 50)
  expect_equal(names(vpc_vanc$sim), c("ID", "TIME", "DV", "ITER"))
  expect_false(any(is.na(vpc_vanc$sim$DV)))

  # Plot:
  expect_s3_class(plot(vpc_vanc), "gg")
  expect_doppelganger("nm_vanco VPC", plot(vpc_vanc, show = list(obs_dv = TRUE)))
})

test_that("Busulfan vpc with vanco data + model works", {
  local_mipdeval_options()

  ## use specific parameters, not model lib
  mod_obj <- parse_model("pkbusulfanshukla")
  vpc_bu <- run_eval(
    model = "pkbusulfanshukla",
    parameters = mod_obj$parameters,
    data = nm_busulfan,
    .vpc_options = vpc_options(n_samples = 100, vpc_only = TRUE),
    progress = FALSE
  )

  # Simulation:
  expect_equal(nrow(vpc_bu$sim), 20000)
  expect_equal(nrow(dplyr::filter(vpc_bu$data, EVID == 0)), 200)
  expect_equal(names(vpc_bu$sim), c("ID", "TIME", "DV", "ITER"))
  expect_false(any(is.na(vpc_bu$sim$DV)))

  # Plot:
  expect_s3_class(plot(vpc_bu), "gg")
  expect_doppelganger(
    "nm_busulfan VPC",
    plot(
      vpc_bu,
      bins = c(unique(nm_busulfan$TIME) - 0.01, 96),
      show = list(obs_dv = TRUE)
    ) # + ggplot2::scale_y_log10()
  )
})
