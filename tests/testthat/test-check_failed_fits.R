test_that("check_failed_fits() warns and returns failed rows", {
  res <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    apriori = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    dv = c(10, 12, NA, NA, 8, 14),
    pred = c(9, 13, NA, NA, 7, 15),
    map_ipred = c(10, 11, NA, NA, 9, 13),
    iter_ipred = c(11, 12, NA, NA, 8, 14)
  )
  expect_warning(
    errors <- check_failed_fits(res),
    "Errors were encountered in 2 out of 6 evaluated predictions"
  )
  expect_equal(nrow(errors), 2)
  expect_equal(unique(errors$id), 2)
})

test_that("check_failed_fits() does not warn when there are no failures", {
  res <- data.frame(
    id = c(1, 1, 2, 2),
    apriori = c(FALSE, TRUE, FALSE, TRUE),
    dv = c(10, 12, 8, 14),
    pred = c(9, 13, 7, 15),
    map_ipred = c(10, 11, 9, 13),
    iter_ipred = c(11, 12, 8, 14)
  )
  expect_no_warning(errors <- check_failed_fits(res))
  expect_equal(nrow(errors), 0)
})
