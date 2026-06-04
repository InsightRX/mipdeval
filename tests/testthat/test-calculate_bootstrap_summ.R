# calculate_bootstrap_summ() --------------------------------------------------
test_that("calculate_bootstrap_summ() evaluates pred vs iter_ipred by apriori", {
  results <- data.frame(
    id = rep(1:10, each = 2),
    apriori = rep(c(TRUE, FALSE), 10),
    dv = rnorm(20, 10, 2),
    pred = rnorm(20, 10, 2),
    iter_ipred = rnorm(20, 10, 2)
  )
  summ <- calculate_bootstrap_summ(
    results,
    error_metrics = c("rmse", "mpe"),
    n_boots = 50,
    seed = 1
  )
  expect_s3_class(summ, "mipdeval_results_bootstrap_summ")
  expect_true("apriori" %in% names(summ))
  expect_equal(nrow(summ), 2) # one row per apriori value
  expect_true(all(c("rmse_mid", "rmse_lower", "rmse_upper") %in% names(summ)))
})

test_that("calculate_bootstrap_summ() requires accuracy margins for accuracy", {
  results <- data.frame(
    apriori = c(TRUE, FALSE), dv = c(10, 10), pred = c(9, 9), iter_ipred = c(11, 11)
  )
  expect_error(
    calculate_bootstrap_summ(results, error_metrics = "accuracy"),
    "must both be specified"
  )
  expect_no_error(
    calculate_bootstrap_summ(
      results, error_metrics = "accuracy",
      acc_error_abs = 0.5, acc_error_rel = 0.25, n_boots = 5
    )
  )
})

# bootstrap_options() ---------------------------------------------------------
test_that("bootstrap_options() validates and stores its inputs", {
  opts <- bootstrap_options(error_metrics = c("rmse", "mpe"), n_boots = 200)
  expect_s3_class(opts, "mipdeval_bootstrap_options")
  expect_equal(opts$error_metrics, c("rmse", "mpe"))
  expect_equal(opts$n_boots, 200)
  expect_true(opts$skip)

  expect_error(bootstrap_options(extra = 1), "must be empty")
  expect_error(bootstrap_options(error_metrics = "nope"))
  expect_error(bootstrap_options(skip = "yes"))
})

test_that("bootstrap_options() does not bootstrap accuracy by default", {
  # accuracy requires error margins, so it is opt-in.
  expect_false("accuracy" %in% bootstrap_options()$error_metrics)
  expect_equal(bootstrap_options()$error_metrics, c("rmse", "nrmse", "mpe", "mape"))
  # but it can still be requested explicitly:
  expect_true("accuracy" %in% bootstrap_options(error_metrics = "accuracy")$error_metrics)
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

