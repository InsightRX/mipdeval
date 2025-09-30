# wrapper for purrr::map() that supports parallel processing via
# furrr::future_map(), as well as optional skipping.
run <- function(.x, .f, ..., .threads = 1, .skip = FALSE) {
  if (isTRUE(.skip)) {
    return()
  } else if (.threads > 1) {
    # TODO: consider using purrr::in_parallel() in the future when it's stable.
    future::plan(future::multisession, workers = .threads)
    res <- purrr::list_rbind(furrr::future_map(
      .x = .x, .f = .f, ...
    ))
  } else {
    res <- purrr::list_rbind(purrr::map(
      .x = .x, .f = .f, ...
    ))
  }
}
