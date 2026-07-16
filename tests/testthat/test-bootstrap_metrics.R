# bootstrap_metrics() ---------------------------------------------------------
test_that("bootstrap_metrics()/summarise_bootstrap_metrics() work without a grouping variable", {
  withr::with_seed(99, {
    dat <- data.frame(
      id = 1:1000, res = rnorm(1000, 2, 3), tdm = rnorm(1000, 10, 5)
    )
  })

  # bootstrap_metrics():
  out <- bootstrap_metrics(dat, rmse = rmse(tdm, tdm - res), .n_boots = 10)

  expect_equal(colnames(out), c("boot", "rmse"))
  expect_equal(round(mean(out$rmse), 1), 3.6)
  expect_equal(nrow(out), 10) # one row per boot
  expect_equal(sort(unique(out$boot)), 1:10) # each boot is numbered

  # summarise_bootstrap_metrics():
  out <- summarise_bootstrap_metrics(out)

  expect_equal(colnames(out), c("rmse_mid", "rmse_lower", "rmse_upper"))
  expect_equal(round(out$rmse_mid, 2), 3.57)
  expect_equal(round(out$rmse_lower, 2), 3.45)
  expect_equal(round(out$rmse_upper, 2), 3.70)
  # lower bound should not exceed mid, which should not exceed upper
  expect_true(all(out$rmse_lower <= out$rmse_mid))
  expect_true(all(out$rmse_mid <= out$rmse_upper))
})

test_that("bootstrap_metrics()/summarise_bootstrap_metrics() work with a grouping variable", {
  withr::with_seed(99, {
    dat <- data.frame(
      id = 1:1000,
      res = rnorm(1000, 2, 3),
      tdm = rnorm(1000, 10, 5),
      group = rep(c("A", "B"), each = 500)
    )
  })

  # bootstrap_metrics():
  out <- bootstrap_metrics(
    dat, rmse = rmse(tdm, tdm - res), .by = group, .n_boots = 10
  )

  expect_equal(colnames(out), c("boot", "group", "rmse"))
  expect_equal(round(mean(out$rmse[out$group == "A"]), 1), 3.5)
  expect_equal(round(mean(out$rmse[out$group == "B"]), 1), 3.7)
  expect_equal(nrow(out), 20) # one row per boot x group
  expect_equal(sort(unique(out$boot)), 1:10) # each boot is numbered

  # summarise_bootstrap_metrics():
  out <- summarise_bootstrap_metrics(out, .by = group)

  expect_equal(colnames(out), c("group", "rmse_mid", "rmse_lower", "rmse_upper"))
  expect_equal(round(out$rmse_mid[out$group == "A"], 2), 3.54)
  expect_equal(round(out$rmse_lower[out$group == "A"], 2), 3.39)
  expect_equal(round(out$rmse_upper[out$group == "A"], 2), 3.62)
  expect_equal(round(out$rmse_mid[out$group == "B"], 2), 3.70)
  expect_equal(round(out$rmse_lower[out$group == "B"], 2), 3.58)
  expect_equal(round(out$rmse_upper[out$group == "B"], 2), 3.85)
  expect_equal(nrow(out), 2) # one row per group
})

test_that("bootstrap_metrics()/summarise_bootstrap_metrics() work with multiple grouping variables", {
  withr::with_seed(99, {
    dat <- data.frame(
      id = 1:1000,
      res = rnorm(1000, 2, 3),
      tdm = rnorm(1000, 10, 5),
      group = rep(c("A", "B"), each = 500),
      model = rep(c("a", "b", "a", "b"), each = 250)
    )
  })

  # bootstrap_metrics():
  out <- bootstrap_metrics(
    dat, rmse = rmse(tdm, tdm - res), .by = c(group, model), .n_boots = 10
  )

  expect_equal(colnames(out), c("boot", "group", "model", "rmse"))
  expect_equal(round(mean(out$rmse[out$group == "A" & out$model == "a"]), 1), 3.5)
  expect_equal(round(mean(out$rmse[out$group == "A" & out$model == "b"]), 1), 3.6)
  expect_equal(round(mean(out$rmse[out$group == "B" & out$model == "a"]), 1), 3.6)
  expect_equal(round(mean(out$rmse[out$group == "B" & out$model == "b"]), 1), 3.6)
  expect_equal(nrow(out), 40) # one row per boot x group
  expect_equal(sort(unique(out$boot)), 1:10) # each boot is numbered

  # summarise_bootstrap_metrics():
  out <- summarise_bootstrap_metrics(out, .by = c(group, model))

  expect_equal(colnames(out), c("group", "model", "rmse_mid", "rmse_lower", "rmse_upper"))
  expect_equal(round(out$rmse_mid[out$group == "A" & out$model == "a"], 2), 3.50)
  expect_equal(round(out$rmse_lower[out$group == "A" & out$model == "a"], 2), 3.25)
  expect_equal(round(out$rmse_upper[out$group == "A" & out$model == "a"], 2), 3.65)
  expect_equal(round(out$rmse_mid[out$group == "B" & out$model == "a"], 2), 3.64)
  expect_equal(round(out$rmse_lower[out$group == "B" & out$model == "a"], 2), 3.18)
  expect_equal(round(out$rmse_upper[out$group == "B" & out$model == "a"], 2), 3.95)
  expect_equal(round(out$rmse_mid[out$group == "A" & out$model == "b"], 2), 3.63)
  expect_equal(round(out$rmse_lower[out$group == "A" & out$model == "b"], 2), 3.50)
  expect_equal(round(out$rmse_upper[out$group == "A" & out$model == "b"], 2), 3.93)
  expect_equal(round(out$rmse_mid[out$group == "B" & out$model == "b"], 2), 3.64)
  expect_equal(round(out$rmse_lower[out$group == "B" & out$model == "b"], 2), 3.48)
  expect_equal(round(out$rmse_upper[out$group == "B" & out$model == "b"], 2), 3.81)
  expect_equal(nrow(out), 4) # one row per unique group
})

test_that("bootstrap_metrics() is reproducible given a seed", {
  df <- data.frame(obs = rnorm(100), pred = rnorm(100))
  a <- bootstrap_metrics(df, rmse = rmse(obs, pred), .seed = 42, .n_boots = 20)
  b <- bootstrap_metrics(df, rmse = rmse(obs, pred), .seed = 42, .n_boots = 20)
  expect_equal(a, b)
})

test_that("bootstrap_metrics() works with all error metrics", {
  withr::with_seed(99, {
    df <- data.frame(
      id = rep(1:1000, 4),
      model = rep(c(rep("A", 1000), rep("B", 1000)), 2),
      patient_type = "general",
      prediction_type = c(rep("a priori", 2000), rep("a posteriori", 2000)),
      res = c(
        rnorm(1000, 2, 3),
        rnorm(1000, 0.1, 1),
        rnorm(1000, 1, 3),
        rnorm(1000, 0, 0.5)
      ),
      tdm = rnorm(4000, 10, 5)
    )
  })
  out <- bootstrap_metrics(
    df,
    rmse = rmse(tdm, tdm - res),
    nrmse = nrmse(tdm, tdm - res),
    mpe = mpe(tdm, tdm - res),
    mape = mape(tdm, tdm - res),
    accuracy = accuracy(tdm, tdm - res, error_abs = 0.5, error_rel = 0.5),
    .by = c(model, patient_type, prediction_type),
    .n_boots = 100
  )

  # Check column names:
  expect_equal(
    colnames(out),
    c(
      "boot",
      "model",
      "patient_type",
      "prediction_type",
      "rmse",
      "nrmse",
      "mpe",
      "mape",
      "accuracy"
    )
  )

  # Check rmse:
  expect_equal(
    round(mean(out$rmse[out$model == "A" & out$prediction_type == "a priori"]), 1), 3.6
  )
  expect_equal(
    round(mean(out$rmse[out$model == "A" & out$prediction_type == "a posteriori"]), 1), 3.2
  )
  expect_equal(
    round(mean(out$rmse[out$model == "B" & out$prediction_type == "a posteriori"]), 1), 0.5
  )

  # Check nrmse:
  expect_equal(round(mean(out$nrmse[out$model == "A" & out$prediction_type == "a priori"]), 1), 0.4)
  expect_equal(round(mean(out$nrmse[out$model == "A" & out$prediction_type == "a posteriori"]), 1), 0.3)
  expect_equal(round(mean(out$nrmse[out$model == "B" & out$prediction_type == "a posteriori"]), 1), 0)

  # Check mpe:
  expect_equal(round(mean(out$mpe[out$model == "A" & out$prediction_type == "a posteriori"]), 1), 0.1)
  expect_equal(round(mean(out$mpe[out$model == "B" & out$prediction_type == "a priori"]), 2), -0.03)
  expect_equal(round(mean(out$mpe[out$model == "B" & out$prediction_type == "a posteriori"]), 2), 0.01)

  # Check mape:
  expect_equal(
    round(mean(out$mape[out$model == "A" & out$prediction_type == "a priori"]), 1), 0.3
  )
  expect_equal(
    round(mean(out$mape[out$model == "A" & out$prediction_type == "a posteriori"]), 1), 0.3
  )
  expect_equal(
    round(mean(out$mape[out$model == "B" & out$prediction_type == "a posteriori"]), 1), 0.1
  )

  # Check accuracy:
  expect_equal(
    round(mean(out$accuracy[out$model == "A" & out$prediction_type == "a priori"]), 1), 0.7
  )
  expect_equal(
    round(mean(out$accuracy[out$model == "A" & out$prediction_type == "a posteriori"]), 1), 0.8
  )
  expect_equal(
    round(mean(out$accuracy[out$model == "B" & out$prediction_type == "a posteriori"]), 1), 1
  )
})

# summarise_bootstrap_metrics() -----------------------------------------------
test_that("summarise_bootstrap_metrics() respects .conf_level", {
  df <- data.frame(obs = rnorm(200), pred = rnorm(200), g = "a")
  boots <- bootstrap_metrics(df, rmse = rmse(obs, pred), .by = g, .seed = 7, .n_boots = 500)
  wide <- summarise_bootstrap_metrics(boots, .by = g, .conf_level = 0.95)
  narrow <- summarise_bootstrap_metrics(boots, .by = g, .conf_level = 0.50)
  # a wider confidence level gives a wider interval:
  expect_gt(
    wide$rmse_upper - wide$rmse_lower,
    narrow$rmse_upper - narrow$rmse_lower
  )
})

test_that("summarise_bootstrap_metrics() rejects an out-of-range .conf_level", {
  boots <- bootstrap_metrics(
    data.frame(obs = rnorm(50), pred = rnorm(50)),
    rmse = rmse(obs, pred), .n_boots = 10
  )
  expect_error(summarise_bootstrap_metrics(boots, .conf_level = 95), "between 0 and 1")
  expect_error(summarise_bootstrap_metrics(boots, .conf_level = 0), "between 0 and 1")
  expect_error(
    summarise_bootstrap_metrics(boots, .conf_level = c(0.9, 0.95)),
    "`.conf_level` must have size 1, not size 2."
  )
})

test_that("summarize_bootstrap_metrics() is an alias", {
  expect_identical(summarize_bootstrap_metrics, summarise_bootstrap_metrics)
})
