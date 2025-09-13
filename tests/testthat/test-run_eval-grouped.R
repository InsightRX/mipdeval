test_that("Grouped run for busulfan/shukla works", {
  n_ids <- 5
  ## TODO: update busulfan dataset to have sampled covariates
  nm_busulfan <- add_grouping_bins(
    nm_busulfan,
    label = "group",
    bins = c(0, 24, 48, 72, 120)
  )
  res <- run_eval(
    model = "pkbusulfanucsf",
    data = nm_busulfan,
    group = "group",
    progress = FALSE,
    ids = c(1:n_ids)
  )
  expect_equal(names(res), c("results", "stats"))

  ## Reference results from PsN proseval:
  raw_proseval <- read.csv(
    file = system.file(
      package = "mipdeval",
      "extdata/busulfan_shukla.csv"
    )
  ) |>
    dplyr::left_join(
      nm_busulfan |>
        dplyr::select(ID, TIME, group), by = c("ID", "TIME")
    )
  proseval <- parse_psn_proseval_results(
    raw_proseval |>
      dplyr::filter(ID <= n_ids),
    group = "group"
  )

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  expect_equal(
    nrow(proseval),
    nrow(res$results |> dplyr::filter(!apriori))
  )

  ## comparable iterative predictions
  expect_true(
    compare_psn_proseval_results(
      res,
      proseval,
      tol = 0.01
    )$within_tol
  )
})
