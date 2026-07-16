test_that("Root mean squared error", {
  pred <- c(7, 10, 12, 10, 10, 8, 7,  8, 11, 13, 10, 8)
  obs  <- c(6, 10, 14, 16,  7, 5, 5, 13, 12, 13,  8, 5)
  expect_equal(round(rmse(obs, pred), 3), 2.915)
})

test_that("Normalized root mean squared error", {
  pred <- c(7, 10, 12, 10, 10, 8, 7,  8, 11, 13, 10, 8)
  obs  <- c(6, 10, 14, 16,  7, 5, 5, 13, 12, 13,  8, 5)
  mean_obs <- mean(obs)
  expect_equal(nrmse(obs, pred), sqrt(8.5)/ mean_obs)
})

test_that("Mean absolute percentage error", {
  pred <- c(7, 10, 12, 10, 10, 8, 7,  8, 11, 13, 10, 8)
  obs  <- c(6, 10, 14, 16,  7, 5, 5, 13, 12, 13,  8, 5)
  expect_equal(round(mape(obs, pred), 3), 0.286)
})

test_that("Mean percentage error", {
  pred <- c(7, 10, 12, 10, 10, 8, 7,  8, 11, 13, 10, 8)
  obs  <- c(6, 10, 14, 16,  7, 5, 5, 13, 12, 13,  8, 5)
  expect_equal(round(mpe(obs, pred), 3), -0.122)
})

test_that("Accuracy", {
  obs <- 6
  pred <- 5

  # No observations should be considered accurate when the error margin is 0
  # (the default):
  expect_false(is_accurate_abs(5, 5, error_abs = 0))
  expect_false(is_accurate_rel(5, 5, error_rel = 0))
  expect_false(is_accurate(obs, pred, error_abs = 0, error_rel = 0))
  expect_false(is_accurate(obs, pred))

  expect_true(is_accurate(obs, pred, error_abs = 2, error_rel = 0))
  expect_false(is_accurate(obs, pred, error_abs = 0.5, error_rel = 0))
  expect_true(is_accurate(obs, pred, error_abs = 0, error_rel = 0.5))
  expect_false(is_accurate(obs, pred, error_abs = 0, error_rel = 0.05))

  expect_equal(
    accuracy(
      c(1, 2, 3, 4, 5),
      c(1.1, 2.5, 3.1, 4.5, 5.1),
      error_abs = 0.2,
      error_rel = 0.05
    ),
    0.6
  )
})

test_that("error metrics ignore NA pairs", {
  pred_na <- c(7, 10, NA, 12)
  obs_na  <- c(6, 10, NA, 14)
  pred    <- pred_na[!is.na(pred_na)]
  obs     <- obs_na[!is.na(obs_na)]

  # NAs are dropped, not propagated to the result:
  expect_false(is.na(rmse(obs_na, pred_na)))
  expect_false(is.na(nrmse(obs_na, pred_na)))
  expect_false(is.na(mpe(obs_na, pred_na)))
  expect_false(is.na(mape(obs_na, pred_na)))
  expect_false(is.na(
    accuracy(obs_na, pred_na, error_abs = 0.2, error_rel = 0.05)
  ))

  # Dropping NAs should match computing the metric on complete pairs:
  expect_equal(rmse(obs_na, pred_na), rmse(obs, pred))
  expect_equal(nrmse(obs_na, pred_na), nrmse(obs, pred))
  expect_equal(mpe(obs_na, pred_na), mpe(obs, pred))
  expect_equal(mape(obs_na, pred_na), mape(obs, pred))
  expect_equal(
    accuracy(obs_na, pred_na, error_abs = 0.2, error_rel = 0.05),
    accuracy(obs, pred, error_abs = 0.2, error_rel = 0.05)
  )
})
