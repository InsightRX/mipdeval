test_that("calculate_bayesian_impact returns correct structure", {
  # Create test data
  test_data <- data.frame(
    type = c("pred", "iter_ipred", "pred", "iter_ipred"),
    apriori = c(FALSE, FALSE, TRUE, TRUE),
    rmse = c(10, 5, 15, 8),
    mape = c(20, 10, 25, 12)
  )
  
  result <- calculate_bayesian_impact(test_data)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("bi_rmse", "bi_mape"))
  expect_equal(nrow(result), 1)
})

test_that("calculate_bayesian_impact calculates correct values", {
  # Test data with known expected results
  test_data <- data.frame(
    type = c("pred", "iter_ipred"),
    apriori = c(FALSE, FALSE),
    rmse = c(10, 5),  # iter_ipred is 50% better
    mape = c(20, 10)  # iter_ipred is 50% better
  )
  
  result <- calculate_bayesian_impact(test_data)
  
  # BI = 100 * (1 - (iter_ipred / pred))
  # For RMSE: 100 * (1 - (5/10)) = 50%
  # For MAPE: 100 * (1 - (10/20)) = 50%
  expect_equal(result$bi_rmse, 50)
  expect_equal(result$bi_mape, 50)
})

test_that("calculate_bayesian_impact handles negative impact correctly", {
  # Test case where Bayesian updating performs worse
  test_data <- data.frame(
    type = c("pred", "iter_ipred"),
    apriori = c(FALSE, FALSE),
    rmse = c(5, 10),   # iter_ipred is worse
    mape = c(10, 20)   # iter_ipred is worse
  )
  
  result <- calculate_bayesian_impact(test_data)
  
  # BI = 100 * (1 - (10/5)) = -100%
  expect_equal(result$bi_rmse, -100)
  expect_equal(result$bi_mape, -100)
})

test_that("calculate_bayesian_impact filters apriori correctly", {
  # Include apriori = TRUE rows that should be filtered out
  test_data <- data.frame(
    type = c("pred", "iter_ipred", "pred", "iter_ipred"),
    apriori = c(TRUE, TRUE, FALSE, FALSE),
    rmse = c(100, 50, 10, 5),
    mape = c(200, 100, 20, 10)
  )
  
  result <- calculate_bayesian_impact(test_data)
  
  # Should only use the apriori = FALSE rows (10, 5 and 20, 10)
  expect_equal(result$bi_rmse, 50)
  expect_equal(result$bi_mape, 50)
})

test_that("calculate_bayesian_impact handles edge cases", {
  # Test with identical values (no improvement)
  test_data <- data.frame(
    type = c("pred", "iter_ipred"),
    apriori = c(FALSE, FALSE),
    rmse = c(10, 10),
    mape = c(20, 20)
  )
  
  result <- calculate_bayesian_impact(test_data)
  
  expect_equal(result$bi_rmse, 0)
  expect_equal(result$bi_mape, 0)
})

test_that("calculate_bayesian_impact handles division by zero", {
  # Test with pred values of zero
  test_data <- data.frame(
    type = c("pred", "iter_ipred"),
    apriori = c(FALSE, FALSE),
    rmse = c(0, 5),
    mape = c(0, 10)
  )
  
  # This should handle division by zero gracefully
  expect_warning({
    result <- calculate_bayesian_impact(test_data)
  }, NA) # No warning expected, but Inf values are possible
  
  result <- calculate_bayesian_impact(test_data)
  expect_true(is.infinite(result$bi_rmse) || is.nan(result$bi_rmse))
  expect_true(is.infinite(result$bi_mape) || is.nan(result$bi_mape))
})

test_that("calculate_bayesian_impact requires correct column names", {
  # Missing required columns
  incomplete_data <- data.frame(
    type = c("pred", "iter_ipred"),
    rmse = c(10, 5)
    # Missing apriori and mape columns
  )
  
  expect_error(calculate_bayesian_impact(incomplete_data))
})
