# add_grouping_column() -------------------------------------------------------
test_that("`fun` can be passed as a symbol, string, or anonymous function", {
  dat <- dplyr::filter(nm_busulfan, ID <= 3)
  out_1 <- add_grouping_column(dat, fun = group_by_time, label = "group")
  out_2 <- add_grouping_column(dat, fun = "group_by_time", label = "group")
  out_3 <- add_grouping_column(
    dat,
    fun = function(data) {
      as.numeric(cut(
        data$TIME, breaks = c(0, 24, 48, 72, 96, Inf), include.lowest = TRUE
      ))
    },
    label = "group"
  )
  expect_identical(out_1, out_2)
  expect_identical(out_1, out_3)
})

test_that("new column name matches `label`", {
  dat <- dplyr::filter(nm_busulfan, ID <= 3)
  out <- add_grouping_column(dat, fun = group_by_time, label = "supergroup")
  expect_true("supergroup" %in% names(out))
})

test_that("alert is thrown when grouping column already exists", {
  dat <- dplyr::mutate(nm_busulfan, GROUP = 1)
  expect_message(
    add_grouping_column(
      dplyr::filter(dat, ID <= 3),
      fun = group_by_dose,
      label = "GROUP"
    ),
    "Overwriting GROUP column in `data`"
  )
})

# group_by_dose() -------------------------------------------------------------
test_that("grouping by dose works properly on 4 dose intervals", {
  nm <- dplyr::filter(nm_busulfan, ID <= 3)
  vec <- group_by_dose(nm)
  dat <- add_grouping_column(nm, fun = "group_by_dose", label = "group")
  res <- c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
           3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
           2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L,
           4L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
           3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L)
  expect_equal(length(vec), 72)
  expect_equal(nrow(dat), 72)
  expect_equal(length(vec), nrow(dat))
  expect_equal(ncol(dat), ncol(nm_busulfan) + 1)
  expect_equal(vec, res)
  expect_equal(dat$group, res)
})

test_that("grouping by dose works properly on 4 intervals when not all have samples", {
  # No samples for 2nd dose:
  busulfan2 <- dplyr::filter(
    nm_busulfan, !(EVID == 0 & TIME > 20 & TIME < 48) & ID <= 3
  )
  vec <- group_by_dose(busulfan2)
  dat <- add_grouping_column(busulfan2, fun = "group_by_dose", label = "group")
  res <- c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
           3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,
           2L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
           2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L)
  expect_equal(vec, res)
  expect_equal(dat$group, res)
})

# group_by_time() -------------------------------------------------------------
test_that("grouping by dose works properly on 4 dose intervals", {
  nm <- dplyr::filter(nm_busulfan, ID <= 3)
  vec <- group_by_time(nm)
  dat <- add_grouping_column(nm, fun = "group_by_time", label = "group")
  res <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4,
           4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3,
           3, 3, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3,
           3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
  expect_equal(length(vec), 72)
  expect_equal(nrow(dat), 72)
  expect_equal(length(vec), nrow(dat))
  expect_equal(ncol(dat), ncol(nm_busulfan) + 1)
  expect_equal(vec, res)
  expect_equal(dat$group, res)
})
