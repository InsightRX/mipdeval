# stats_summ_options() --------------------------------------------------------
test_that("stats_summ_options() works", {
  actual <- stats_summ_options(
    rounding = 2, acc_error_abs = 3, acc_error_rel = 4
  )
  expected <- list(rounding = 2, acc_error_abs = 3, acc_error_rel = 4)
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
