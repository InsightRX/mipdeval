require("pkbusulfanshukla")
library(dplyr)

test_that("Grouped run for busulfan/shukla works", {
  nm_busulfan <- add_grouping_bins(
    nm_busulfan,
    label = "group",
    bins = c(0, 24, 48, 72, 120)
  )
  res <- run_eval(
    model = "pkbusulfanshukla",
    data = nm_busulfan,
    group = "group",
    progress = FALSE,
    ids = c(1:3)
  )
  expect_equal(names(res), c("results", "stats"))

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  # expect_equal(
  #   nrow(proseval),
  #   nrow(res$results |> filter(!apriori))
  # )
  # ## comparable iterative predictions
  # comp <- res$results |>
  #   filter(!apriori) |>
  #   mutate(proseval = proseval$pred) |>
  #   mutate(
  #     df = iter_ipred - proseval,
  #     rel_diff = df / proseval
  #   ) |>
  #   arrange(rel_diff)
  # expect_true(all(abs(comp$rel_diff) < 0.10))

})
