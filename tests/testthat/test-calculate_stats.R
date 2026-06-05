# calculate_stats() -----------------------------------------------------------
test_that("calculate_stats() works", {
  res <- data.frame(
    id = c(1, 1, 2, 2),
    apriori = c(FALSE, TRUE, FALSE, TRUE),
    dv = c(10, 12, 8, 14),
    pred = c(9, 13, 7, 15),
    map_ipred = c(10, 11, 9, 13),
    iter_ipred = c(11, 12, 8, 14)
  )
  out <- calculate_stats(res)

  # One row per prediction type x apriori group:
  expect_setequal(out$type, c("pred", "map_ipred", "iter_ipred"))
  expect_setequal(out$apriori, c(TRUE, FALSE))
  expect_equal(nrow(out), 6)

  # Ensure that error metrics are computed separately apriori and aposteriori:
  for (.apriori in c(TRUE, FALSE)) {
    out_pred <- out[out$type == "pred" & out$apriori == .apriori, ]
    res2 <- res[res$apriori == .apriori, ]
    expect_equal(out_pred$rmse, round(rmse(res2$dv, res2$pred), 3))
    expect_equal(out_pred$nrmse, round(nrmse(res2$dv, res2$pred), 3))
    expect_equal(out_pred$mpe, round(mpe(res2$dv, res2$pred), 3))
    expect_equal(out_pred$mape, round(mape(res2$dv, res2$pred), 3))
    expect_true(is.na(out_pred$accuracy)) # NA by default
  }

  expect_s3_class(out, "mipdeval_results_stats_summ")
  expect_s3_class(out, "tbl_df")
})

test_that("calculate_stats() warns when predictions contain errors (NAs)", {
  res <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    apriori = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    dv = c(10, 12, NA, NA, 8, 14),
    pred = c(9, 13, NA, NA, 7, 15),
    map_ipred = c(10, 11, NA, NA, 9, 13),
    iter_ipred = c(11, 12, NA, NA, 8, 14)
  )
  expect_warning(
    out <- calculate_stats(res, acc_error_abs = 0.2, acc_error_rel = 0.05),
    "Errors were encountered in 2 out of 6 evaluated predictions"
  )

  # Patients with errors should be ignored and metrics should be calculated:
  expect_all_false(is.na(out$rmse))
  expect_all_false(is.na(out$nrmse))
  expect_all_false(is.na(out$mpe))
  expect_all_false(is.na(out$mape))
  expect_all_false(is.na(out$accuracy))
})

test_that("calculate_stats(warn = FALSE) does not warn about failed fits", {
  res <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    apriori = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    dv = c(10, 12, NA, NA, 8, 14),
    pred = c(9, 13, NA, NA, 7, 15),
    map_ipred = c(10, 11, NA, NA, 9, 13),
    iter_ipred = c(11, 12, NA, NA, 8, 14)
  )
  expect_no_warning(
    calculate_stats(res, acc_error_abs = 0.2, acc_error_rel = 0.05, warn = FALSE)
  )
})

# stats_summ_options() --------------------------------------------------------
test_that("stats_summ_options() works", {
  actual <- stats_summ_options(
    rounding = 2, acc_error_abs = 3, acc_error_rel = 4
  )
  expected <- list(
    rounding = 2, acc_error_abs = 3, acc_error_rel = 4,
    bootstrap = bootstrap_options()
  )
  expect_s3_class(actual, "mipdeval_stats_summ_options")
  expect_identical(unclass(actual), expected)
})

test_that("stats_summ_options() throws error for incomplete accuracy args", {
  expect_error(
    stats_summ_options(rounding = 2, acc_error_abs = NULL, acc_error_rel = 4),
    "`acc_error_abs` is NULL"
  )
  expect_error(
    stats_summ_options(rounding = 2, acc_error_abs = 3, acc_error_rel = NULL),
    "`acc_error_rel` is NULL"
  )
  expect_no_error(
    stats_summ_options(rounding = 2, acc_error_abs = NULL, acc_error_rel = NULL)
  )
})

test_that("stats_summ_options() nests and validates bootstrap options", {
  expect_s3_class(stats_summ_options()$bootstrap, "mipdeval_bootstrap_options")
  expect_true(stats_summ_options()$bootstrap$skip) # skip bootstrap by default
  expect_error(
    stats_summ_options(bootstrap = list(skip = FALSE)),
    "must be the result of a call to"
  )
})
