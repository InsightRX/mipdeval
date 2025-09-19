# check_dictionary_columns() --------------------------------------------------
test_that("check_dictionary_columns() works", {
  dat <- data.frame(id = 1, time = 0, evid = 0, dv = 10)
  dict_good <- list(ID = "id")
  dict_bad <- list(ID = "ud")
  dict_bad_plural <- list(ID = "ud", EVID = "evud")
  expect_null(check_dictionary_columns(dat, dict_good))
  expect_error(check_dictionary_columns(dat, dict_bad), "must exist in `data`")
  # cli pluralization:
  expect_error(check_dictionary_columns(dat, dict_bad), "is not a column")
  expect_error(check_dictionary_columns(dat, dict_bad_plural), "are not columns")
})

# check_valid_dictionary() ----------------------------------------------------
test_that("check_valid_dictionary() works", {
  dict_good <- list(ID = "id")
  dict_bad <- list(UD = "id")
  dict_bad_plural <- list(UD = "id", EVUD = "evid")
  expect_null(check_valid_dictionary(dict_good))
  expect_error(check_valid_dictionary(dict_bad), "names must be any of the")
  # cli pluralization:
  expect_error(check_valid_dictionary(dict_bad), "is not a valid name")
  expect_error(check_valid_dictionary(dict_bad_plural), "are not valid names")
})

# check_required_cols() -------------------------------------------------------
test_that("check_required_cols() works", {
  dat_good <- data.frame(ID = 1, TIME = 0, EVID = 0, DV = 10)
  dat_bad <- data.frame(ID = 1, EVID = 0, DV = 10)
  dat_bad_plural <- data.frame(ID = 1, EVID = 0)
  expect_null(check_required_cols(dat_good))
  expect_error(check_required_cols(dat_bad), "must have the following columns")
  # cli pluralization:
  expect_error(check_required_cols(dat_bad), "is missing")
  expect_error(check_required_cols(dat_bad_plural), "are missing")
})
