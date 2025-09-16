nm_busulfan <- read.csv(file = "./inst/datasets/nm_busulfan.csv")

## Grouping by dose
test_that("grouping by dose works properly on 4 dose intervals", {
  dat <- add_grouping_column(
    nm_busulfan |> dplyr::filter(ID <= 3),
    fun = "group_by_dose",
    label = "group"
  )
  expect_equal(nrow(dat), 72)
  expect_equal(ncol(dat), ncol(nm_busulfan) + 1)
  expect_equal(
    dat$group,
    c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
      3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
      2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L,
      4L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
      3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L)
  )
})

test_that("grouping by dose works properly on 4 intervals when not all have samples", {
  busulfan2 <- nm_busulfan |>
    dplyr::filter(! (EVID == 0 & TIME > 20 & TIME < 48)) |>
    dplyr::filter(ID <= 3)
  ## (no samples for 2nd dose)
  dat <- add_grouping_column(
    busulfan2,
    fun = "group_by_dose",
    label = "group"
  )
  expect_equal(
    dat$group,
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
      3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,
      2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
      2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L)
  )
})

## Grouping by time
test_that("grouping by dose works properly on 4 dose intervals", {
  dat <- add_grouping_column(
    nm_busulfan |> dplyr::filter(ID <= 3),
    fun = "group_by_time",
    label = "group"
  )
  expect_equal(nrow(dat), 72)
  expect_equal(ncol(dat), ncol(nm_busulfan) + 1)
  expect_equal(
    dat$group,
    c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4,
      4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3,
      3, 3, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3,
      3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
  )
})
