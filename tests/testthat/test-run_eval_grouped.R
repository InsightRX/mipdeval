test_that("Grouped run for busulfan/shukla works", {
  local_mipdeval_options()
  n_ids <- 5
  nm_busulfan <- add_grouping_column(
    nm_busulfan,
    fun = "group_by_dose",
    label = "group"
  )
  res <- run_eval(
    model = "pkbusulfanshukla",
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
  )
  proseval <- raw_proseval |>
    dplyr::left_join(
      dplyr::select(nm_busulfan, ID, TIME, group), by = c("ID", "TIME")
    ) |>
    dplyr::filter(ID <= n_ids) |>
    parse_psn_proseval_results(group = "group")

  ## Check that it matches proseval results in NONMEM / PsN
  ## Same number of rows
  expect_equal(nrow(proseval), nrow(dplyr::filter(res$results, !apriori)))

  ## comparable iterative predictions
  expect_true(compare_psn_proseval_results(res, proseval, tol = 0.01)$within_tol)
})
