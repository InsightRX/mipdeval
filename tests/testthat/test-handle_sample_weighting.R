test_that("handle_sample_weighting returns correct structure", {

  # Create test data
  obs_data <- data.frame(
    id = c(1, 1, 1, 1),
    t = c(0, 12, 24, 48),
    dv = c(10, 8, 6, 4),
    `_grouper` = c(1, 2, 3, 4),
    check.names = FALSE
  )

  iterations <- c(1, 2, 3, 4)

  result <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 2)

  expect_type(result, "double")
  expect_length(result, nrow(obs_data))
  expect_true(all(result %in% c(0, 1)))  # Weights should be 0 or 1
})

test_that("handle_sample_weighting works correctly for non-incremental mode", {
  obs_data <- data.frame(
    `_grouper` = c(1, 2, 3, 4, 5),
    check.names = FALSE
  )

  iterations <- c(1, 2, 3, 4, 5)

  # Test i = 1 (first iteration)
  weights_i1 <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 1)
  expect_equal(weights_i1, c(1, 0, 0, 0, 0))

  # Test i = 3 (third iteration)
  weights_i3 <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 3)
  expect_equal(weights_i3, c(1, 1, 1, 0, 0))

  # Test i = 5 (last iteration)
  weights_i5 <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 5)
  expect_equal(weights_i5, c(1, 1, 1, 1, 1))
})

test_that("handle_sample_weighting works correctly for incremental mode", {
  obs_data <- data.frame(
    `_grouper` = c(1, 2, 3, 4, 5),
    check.names = FALSE
  )

  iterations <- c(1, 2, 3, 4, 5)

  # Test i = 1 (first iteration) - should only use first group
  weights_i1 <- handle_sample_weighting(obs_data, iterations, incremental = TRUE, i = 1)
  expect_equal(weights_i1, c(1, 0, 0, 0, 0))

  # Test i = 3 (third iteration) - should only use third group
  weights_i3 <- handle_sample_weighting(obs_data, iterations, incremental = TRUE, i = 3)
  expect_equal(weights_i3, c(0, 0, 1, 0, 0))

  # Test i = 5 (last iteration) - should only use fifth group
  weights_i5 <- handle_sample_weighting(obs_data, iterations, incremental = TRUE, i = 5)
  expect_equal(weights_i5, c(0, 0, 0, 0, 1))
})

test_that("handle_sample_weighting handles multiple observations per group", {
  # Multiple observations can have the same _grouper value
  obs_data <- data.frame(
    `_grouper` = c(1, 1, 2, 2, 3, 3),
    check.names = FALSE
  )

  iterations <- c(1, 2, 3)

  # Non-incremental: i = 2 should include groups 1 and 2
  weights_non_inc <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 2)
  expect_equal(weights_non_inc, c(1, 1, 1, 1, 0, 0))

  # Incremental: i = 2 should only include group 2
  weights_inc <- handle_sample_weighting(obs_data, iterations, incremental = TRUE, i = 2)
  expect_equal(weights_inc, c(0, 0, 1, 1, 0, 0))
})


test_that("handle_sample_weighting handles edge case with single observation", {
  obs_data <- data.frame(
    `_grouper` = c(1),
    check.names = FALSE
  )

  iterations <- c(1)

  # Both modes should give same result for single observation
  weights_non_inc <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 1)
  weights_inc <- handle_sample_weighting(obs_data, iterations, incremental = TRUE, i = 1)

  expect_equal(weights_non_inc, c(1))
  expect_equal(weights_inc, c(1))
})

test_that("handle_sample_weighting applies weight scheme to active samples", {
  obs_data <- data.frame(
    t = c(0, 12, 24, 48),
    `_grouper` = c(1, 2, 3, 4),
    check.names = FALSE
  )
  iterations <- c(1, 2, 3, 4)

  # At iteration 3, samples 1-3 are active. weight_last_only should

  # give weight 1 only to the most recent active sample (t=24).
  result <- handle_sample_weighting(
    obs_data, iterations, incremental = FALSE, i = 3,
    weights = "weight_last_only"
  )
  expect_equal(result, c(0, 0, 1, 0))
})

test_that("handle_sample_weighting applies exponential weights to active samples", {
  obs_data <- data.frame(
    t = c(0, 24, 48),
    `_grouper` = c(1, 2, 3),
    check.names = FALSE
  )
  iterations <- c(1, 2, 3)

  result <- handle_sample_weighting(
    obs_data, iterations, incremental = FALSE, i = 3,
    weights = list(scheme = "weight_gradient_exponential", t12_decay = 48)
  )
  expect_equal(result[3], 1)
  expect_equal(result[1], 0.5, tolerance = 1e-10)
  expect_true(result[2] > result[1] && result[2] < result[3])
})

test_that("handle_sample_weighting without weights gives binary result", {
  obs_data <- data.frame(
    t = c(0, 12, 24),
    `_grouper` = c(1, 2, 3),
    check.names = FALSE
  )
  iterations <- c(1, 2, 3)

  result <- handle_sample_weighting(
    obs_data, iterations, incremental = FALSE, i = 2,
    weights = NULL
  )
  expect_equal(result, c(1, 1, 0))
})

test_that("handle_sample_weighting maintains correct vector length", {
  # Test with various data sizes
  for(n_obs in c(1, 5, 10, 100)) {
    obs_data <- data.frame(
      `_grouper` = rep(1:min(n_obs, 10), length.out = n_obs),
      check.names = FALSE
    )

    iterations <- unique(obs_data[["_grouper"]])

    weights <- handle_sample_weighting(obs_data, iterations, incremental = FALSE, i = 1)
    expect_length(weights, n_obs)
  }
})
