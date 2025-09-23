test_that("calculate_shrinkage returns correct structure", {
  # Mock mod_obj with required components
  mod_obj <- list(
    parameters = list(cl = 1.5, v = 10.2, ka = 0.8),
    fixed = character(0),  # No fixed parameters
    omega = c(0.1, 0.2, 0.15)  # Triangle format: var(cl), var(v), var(ka)
  )
  
  # Mock res_df with parameter estimates
  res_df <- data.frame(
    `_iteration` = c(1, 1, 2, 2),
    cl = c(1.2, 1.8, 1.0, 2.0),
    v = c(9.5, 11.0, 8.8, 12.1),
    ka = c(0.7, 0.9, 0.6, 1.1),
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_s3_class(result, "data.frame")
  expect_true("_iteration" %in% names(result))
  expect_true(all(c("cl", "v", "ka") %in% names(result)))
  expect_equal(nrow(result), 2)  # Two iterations
})

test_that("calculate_shrinkage handles exponential parameters correctly", {
  mod_obj <- list(
    parameters = list(cl = 2.0, v = 15.0),
    fixed = character(0),
    omega = c(0.09, 0.01, 0.16)  # var(cl) = 0.09, var(v) = 0.16
  )
  
  # Create data where individual estimates vary from population
  res_df <- data.frame(
    `_iteration` = c(1, 1, 1),
    cl = c(2.0, 2.2, 1.8),  # Around pop value of 2.0
    v = c(15.0, 16.5, 13.5),  # Around pop value of 15.0
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$cl))
  expect_true(is.numeric(result$v))
  expect_true(result$cl >= 0 && result$cl <= 100)
  expect_true(result$v >= 0 && result$v <= 100)
})

test_that("calculate_shrinkage handles kappa parameters (additive IIV)", {
  mod_obj <- list(
    parameters = list(kappa_cl = 0.1, kappa_v = 0.05),
    fixed = character(0),
    omega = c(0.01, 0.0025)  # var(kappa_cl), var(kappa_v)
  )
  
  res_df <- data.frame(
    `_iteration` = c(1, 1, 1),
    kappa_cl = c(0.12, 0.08, 0.11),
    kappa_v = c(0.06, 0.04, 0.055),
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$kappa_cl))
  expect_true(is.numeric(result$kappa_v))
})

test_that("calculate_shrinkage handles zero population parameter", {
  mod_obj <- list(
    parameters = list(cl = 1.5, baseline = 0),  # Zero baseline
    fixed = character(0),
    omega = c(0.1, 0.01, 0.05)
  )
  
  res_df <- data.frame(
    `_iteration` = c(1, 1, 1),
    cl = c(1.2, 1.8, 1.0),
    baseline = c(-0.1, 0.1, 0.05),  # Should use additive for zero pop
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$cl))
  expect_true(is.numeric(result$baseline))
})

test_that("calculate_shrinkage handles fixed parameters correctly", {
  mod_obj <- list(
    parameters = list(cl = 1.5, ka = 0.8, v = 10.2),
    fixed = c("ka"),  # ka is fixed
    omega = c(0.1, 0.01, 0.2)  # Only cl and v have random effects
  )
  
  res_df <- data.frame(
    `_iteration` = c(1, 1, 2, 2),
    cl = c(1.2, 1.8, 1.0, 2.0),
    v = c(9.5, 11.0, 8.8, 12.1),
    ka = c(0.8, 0.8, 0.8, 0.8),  # Fixed value
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_true("cl" %in% names(result))
  expect_true("v" %in% names(result))
  expect_false("ka" %in% names(result))  # Should not appear (fixed)
})

test_that("calculate_shrinkage handles multiple iterations", {
  mod_obj <- list(
    parameters = list(cl = 1.5, v = 10.2),
    fixed = character(0),
    omega = c(0.1, 0.01, 0.2)
  )
  
  res_df <- data.frame(
    `_iteration` = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    cl = c(1.2, 1.8, 1.0, 1.6, 1.4, 1.7, 1.1, 1.9, 1.3),
    v = c(9.5, 11.0, 8.8, 10.8, 9.2, 11.5, 8.9, 12.0, 9.8),
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_equal(nrow(result), 3)  # Three iterations
  expect_equal(result$`_iteration`, c(1, 2, 3))
})

test_that("calc_eta helper function works correctly", {
  parameters <- list(cl = 2.0, v = 15.0, kappa_cl = 0.1, baseline = 0)
  
  # Test exponential parameter (cl)
  eta_cl <- calc_eta(2.2, "cl", parameters)
  expect_equal(eta_cl, log(2.2/2.0), tolerance = 1e-10)
  
  # Test kappa parameter (additive)
  eta_kappa <- calc_eta(0.12, "kappa_cl", parameters)
  expect_equal(eta_kappa, 0.12 - 0.1)
  
  # Test zero population parameter (additive)
  eta_baseline <- calc_eta(0.05, "baseline", parameters)
  expect_equal(eta_baseline, 0.05 - 0)
})

test_that("calc_shrinkage helper function works correctly", {
  # Test shrinkage calculation
  omega_list <- list(cl = 0.09)  # omega^2 = 0.09, so omega = 0.3
  eta_values <- c(0.1, -0.1, 0.05, -0.05, 0.02)
  
  shrinkage <- calc_shrinkage(eta_values, "eta_cl", omega_list)
  expected_shrinkage <- 100 * (1 - (sd(eta_values) / sqrt(0.09)))
  
  expect_equal(shrinkage, expected_shrinkage, tolerance = 1e-10)
  expect_true(shrinkage >= -100 && shrinkage <= 100)  # Reasonable range
})

test_that("get_omega_for_parameters works correctly", {
  mod_obj <- list(
    parameters = list(cl = 1.5, v = 10.2, ka = 0.8, f1 = 1.0),
    fixed = c("f1"),  # f1 is fixed
    omega = c(
      0.1,
      0.05, 0.02, 
      0.15, 0.01, 0.25)  # Triangle: 3x3 matrix for cl, v, ka
  )
  
  omega_list <- get_omega_for_parameters(mod_obj)
  
  expect_equal(omega_list, list(cl = 0.1, v = 0.02, ka = 0.25))
})

test_that("calculate_shrinkage handles edge case with no variability", {
  mod_obj <- list(
    parameters = list(cl = 1.5),
    fixed = character(0),
    omega = c(0.1)
  )
  
  # All individual estimates are the same (no variability)
  res_df <- data.frame(
    `_iteration` = c(1, 1, 1),
    cl = c(1.5, 1.5, 1.5),  # No variation
    check.names = FALSE
  )
  
  result <- calculate_shrinkage(res_df, mod_obj)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$cl, 100, tolerance = 1e-10)  # Perfect shrinkage
})

test_that("calculate_shrinkage validates input structure", {
  mod_obj <- list(
    parameters = list(cl = 1.5),
    fixed = character(0),
    omega = c(0.1)
  )
  
  # Missing _iteration column
  bad_res_df <- data.frame(cl = c(1.2, 1.8, 1.0))
  
  expect_error(calculate_shrinkage(bad_res_df, mod_obj))
})
