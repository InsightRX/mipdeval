test_that("weight_all returns all ones", {
  result <- calculate_fit_weights("weight_all", t = c(0, 12, 24, 48))
  expect_equal(result, c(1, 1, 1, 1))
})

test_that("weight_last_only returns 1 for most recent observation", {
  result <- calculate_fit_weights("weight_last_only", t = c(0, 12, 24, 48))
  expect_equal(result, c(0, 0, 0, 1))
})

test_that("weight_last_only handles unsorted times", {
  result <- calculate_fit_weights("weight_last_only", t = c(24, 0, 48, 12))
  expect_equal(result, c(0, 0, 1, 0))
})

test_that("weight_last_two_only returns 1 for two most recent observations", {
  result <- calculate_fit_weights("weight_last_two_only", t = c(0, 12, 24, 48))
  expect_equal(result, c(0, 0, 1, 1))
})

test_that("weight_last_two_only handles unsorted times", {
  result <- calculate_fit_weights("weight_last_two_only", t = c(24, 0, 48, 12))
  expect_equal(result, c(1, 0, 1, 0))
})

test_that("weight_last_two_only works with single observation", {
  result <- calculate_fit_weights("weight_last_two_only", t = c(10))
  expect_equal(result, c(1))
})

test_that("weight_gradient_exponential produces correct decay", {
  t <- c(0, 24, 48)
  result <- calculate_fit_weights("weight_gradient_exponential", t = t)
  # default t12_decay = 48, most recent sample (t=48) gets weight 1
  expect_equal(result[3], 1)
  # t=24 is 24h before max, weight = exp(-log(2)/48 * 24) = exp(-log(2)/2)
  expect_equal(result[2], exp(-log(2) / 2), tolerance = 1e-10)
  # t=0 is 48h before max, weight = exp(-log(2)/48 * 48) = 0.5
  expect_equal(result[1], 0.5, tolerance = 1e-10)
})

test_that("weight_gradient_exponential accepts custom t12_decay", {
  t <- c(0, 24)
  result <- calculate_fit_weights(
    list(scheme = "weight_gradient_exponential", t12_decay = 24),
    t = t
  )
  expect_equal(result[2], 1)
  expect_equal(result[1], 0.5, tolerance = 1e-10)
})

test_that("weight_gradient_exponential respects t_start delay", {
  t <- c(0, 20, 24)
  result <- calculate_fit_weights(
    list(scheme = "weight_gradient_exponential", t12_decay = 48, t_start = 4),
    t = t
  )
  # t=24 is 0h ago, no decay -> weight 1
  expect_equal(result[3], 1)
  # t=20 is 4h ago, within t_start window, t_diff clamped to 0 -> weight 1
  expect_equal(result[2], 1)
  # t=0 is 24h ago, minus t_start=4 -> effective t_diff=20
  expect_equal(result[1], exp(-log(2) / 48 * 20), tolerance = 1e-10)
})

test_that("weight_gradient_linear uses default gradient", {
  t <- c(0, 100, 200)
  result <- calculate_fit_weights("weight_gradient_linear", t = t)
  expect_length(result, 3)
  # most recent sample should get highest weight
  expect_true(result[3] >= result[2])
  expect_true(result[2] >= result[1])
})

test_that("weight_gradient_linear accepts custom gradient via list", {
  result <- calculate_fit_weights(
    list(scheme = "weight_gradient_linear", gradient = list(t1 = 3, w1 = 0.2, t2 = 1, w2 = 0.9)),
    t = c(0, 24, 48, 72)
  )
  expect_length(result, 4)
  expect_equal(result[4], 0.9)
})

test_that("weight_gradient_linear warns when t2 > t1", {
  expect_warning(
    calculate_fit_weights(
      list(scheme = "weight_gradient_linear", gradient = list(t1 = 1, t2 = 5)),
      t = c(0, 24, 48)
    ),
    "t2.*>.*t1"
  )
})

test_that("invalid scheme returns NULL with warning", {
  expect_warning(
    result <- calculate_fit_weights("nonexistent_scheme", t = c(0, 12)),
    "not recognized"
  )
  expect_null(result)
})

test_that("NULL weights returns NULL", {
  expect_null(calculate_fit_weights(NULL, t = c(0, 12)))
})

test_that("NULL t returns NULL", {
  expect_null(calculate_fit_weights("weight_all", t = NULL))
})

test_that("negative times get weight 0", {
  result <- calculate_fit_weights("weight_all", t = c(-10, 0, 12, 24))
  expect_equal(result[1], 0)
  expect_equal(result[2:4], c(1, 1, 1))
})

test_that("scheme passed as list with scheme element works", {
  result <- calculate_fit_weights(list(scheme = "weight_all"), t = c(0, 12))
  expect_equal(result, c(1, 1))
})
