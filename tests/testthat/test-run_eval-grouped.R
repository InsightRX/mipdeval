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

  ## Reference results from PsN proseval:
  raw_proseval <- read.csv(
    file = system.file(
      package = "mipdeval",
      "extdata/busulfan_shukla.csv"
    )
  ) |>
    left_join(
      nm_busulfan |>
        select(ID, TIME, group), by = c("ID", "TIME")
    )
  proseval <- parse_psn_proseval_results(
    raw_proseval |>
      filter(ID <= 3),
    group = "group"
  )

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  expect_equal(
    nrow(proseval),
    nrow(res$results |> filter(!apriori))
  )

  ## comparable iterative predictions
  expect_true(
    compare_psn_proseval_results(
      res,
      proseval,
      tol = 0.25
    )$within_tol
  )

})
