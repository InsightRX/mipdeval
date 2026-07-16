# calculate_bootstrap_summ() --------------------------------------------------
test_that("calculate_bootstrap_summ() evaluates pred vs iter_ipred by apriori", {
  results <- data.frame(
    id = rep(1:10, each = 2),
    apriori = rep(c(TRUE, FALSE), 10),
    dv = rnorm(20, 10, 2),
    pred = rnorm(20, 10, 2),
    iter_ipred = rnorm(20, 10, 2)
  )
  out <- calculate_bootstrap_summ(results, n_boots = 50, seed = 1)
  expect_s3_class(out, "mipdeval_results_bootstrap_summ")
  expect_true("apriori" %in% names(out))
  expect_equal(nrow(out), 2) # one row per apriori value
  # all parameterless metrics are bootstrapped:
  expect_true(all(c("rmse_mid", "rmse_lower", "rmse_upper") %in% names(out)))
  expect_true(all(c("nrmse_mid", "nrmse_lower", "nrmse_upper") %in% names(out)))
  expect_true(all(c("mpe_mid", "mpe_lower", "mpe_upper") %in% names(out)))
  expect_true(all(c("mape_mid", "mape_lower", "mape_upper") %in% names(out)))
  # accuracy is omitted when no error margins are supplied:
  expect_false(all(c("accuracy_mid", "accuracy_lower", "accuracy_upper") %in% names(out)))
})

test_that("calculate_bootstrap_summ() bootstraps accuracy only with both margins", {
  results <- data.frame(
    id = rep(1:10, each = 2),
    apriori = rep(c(TRUE, FALSE), 10),
    dv = rnorm(20, 10, 2),
    pred = rnorm(20, 10, 2),
    iter_ipred = rnorm(20, 10, 2)
  )
  # accuracy is bootstrapped when both margins are supplied:
  out <- calculate_bootstrap_summ(
    results, acc_error_abs = 0.5, acc_error_rel = 0.25, n_boots = 10
  )
  expect_true(all(c("accuracy_mid", "accuracy_lower", "accuracy_upper") %in% names(out)))
  # otherwise error:
  expect_error(
    calculate_bootstrap_summ(results, acc_error_abs = 0.5, n_boots = 10),
    "must both be specified"
  )
})

test_that("calculate_bootstrap_summ() accepts tidy-select or character `.by`", {
  results <- data.frame(
    id = rep(1:10, each = 2),
    apriori = rep(c(TRUE, FALSE), 10),
    dv = rnorm(20, 10, 2),
    pred = rnorm(20, 10, 2),
    iter_ipred = rnorm(20, 10, 2),
    model = rep(c("A", "B"), each = 10)
  )
  tidyselect <- calculate_bootstrap_summ(results, .by = model, n_boots = 10, seed = 1)
  character <- calculate_bootstrap_summ(results, .by = "model", n_boots = 10, seed = 1)

  expect_identical(tidyselect, character)
  expect_true(all(c("apriori", "model") %in% names(tidyselect)))
  expect_equal(nrow(tidyselect), 4) # apriori x model
})

# bootstrap_options() ---------------------------------------------------------
test_that("bootstrap_options() validates and stores its inputs", {
  opts <- bootstrap_options(n_boots = 200, seed = 7)
  expect_s3_class(opts, "mipdeval_bootstrap_options")
  expect_equal(opts$n_boots, 200)
  expect_equal(opts$seed, 7)
  expect_true(opts$skip)

  expect_error(bootstrap_options(extra = 1), "must be empty")
  expect_error(bootstrap_options(skip = "yes"))
})

test_that("bootstrap_options() rejects an out-of-range conf_level", {
  expect_error(bootstrap_options(conf_level = 95), "between 0 and 1")
  expect_error(bootstrap_options(conf_level = 0), "between 0 and 1")
  expect_error(bootstrap_options(conf_level = 1), "between 0 and 1")
  expect_error(
    bootstrap_options(conf_level = c(0.9, 0.95)),
    "`conf_level` must have size 1, not size 2."
  )
  expect_no_error(bootstrap_options(conf_level = 0.9))
})
