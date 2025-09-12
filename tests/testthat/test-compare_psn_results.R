test_that("compare_psn_proseval_results() works", {
  res <- list(
    results = data.frame(
      iter_ipred = c(31.6, 20.4, 18.4, 10.9, 5.46),
      apriori = c(TRUE, rep(FALSE, times = 4))
    )
  )
  proseval <- data.frame(pred = c(20.4, 18.4, 10.9, 5.46))
  out <- compare_psn_proseval_results(res, proseval, tol = 0.1)
  expect_equal(names(out), c("within_tol"))
  expect_true(out$within_tol)
})
