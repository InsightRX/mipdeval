#' Add grouping column using a function
#'
#' @inheritParams run_eval
#'
#' @param fun Function used to define groups. Either (1) a quoted or character
#'   name referencing a function (e.g., [group_by_time()], [group_by_dose()], or a
#'   custom function), or (2) an anonymous function; see examples.
#' @param label column name of grouper column in dataset
#' @param ... Additional arguments passed onto `fun`. See the respective
#'   grouping functions for more details. E.g. for `group_by_time`, need a
#'   `bins` argument.
#'
#' @details
#'
#' All functions supplied to `add_grouping_column()` must take at least the
#' argument `data`, and return a numeric vector (or a vector that can be cast to
#' numeric) of the same length as the number of rows in `data`, which will be
#' used as the grouping column.
#'
#' @returns data.frame
#' @seealso [group_by_time()], [group_by_dose()]
#' @examples
#' # group_by_dose:
#' add_grouping_column(nm_busulfan, fun = group_by_dose, label = "group")
#'
#' # group_by_time:
#' add_grouping_column(nm_busulfan, fun = "group_by_time", label = "group")
#'
#' # Anonymous function:
#' add_grouping_column(
#'   nm_busulfan,
#'   fun = function(data) {
#'     as.numeric(cut(
#'       data$TIME, breaks = c(0, 24, 48, 72, 96, Inf), include.lowest = TRUE
#'     ))
#'   },
#'   label = "group"
#' )
#'
#' @export
add_grouping_column <- function(
  data,
  fun = group_by_time,
  label = "group",
  ...
) {
  if(label %in% names(data)) {
    cli::cli_alert_warning("Overwriting {.field {label}} column in {.arg data}.")
  }
  if(inherits(fun, "character")) {
    fun <- get(fun)
  }
  data[[label]] <- fun(data, ...)
  data
}

#' Group data by time using bin separators
#'
#' @inheritParams add_grouping_column
#' @inheritParams run_eval
#' @param bins vector of bin separators. Suggestion to keep the last bin
#'   separator `Inf`, to ensure all points are included in a group.
#'
#' @returns a numeric vector of the same length as the number of rows in `data`.
#'
#' @examples
#' group_by_time(nm_busulfan)
#'
#' @export
group_by_time <- function(
  data,
  bins = c(0, 24, 48, 72, 96, Inf),
  dictionary = list("TIME" = "TIME"),
  ...
) {
  as.numeric(cut(data[[dictionary$TIME]], breaks = bins, include.lowest = TRUE))
}

#' Will create a separate group for each dose intervals that contains at least
#' one sample
#'
#' @inheritParams add_grouping_column
#' @inheritParams run_eval
#'
#' @returns a numeric vector of the same length as the number of rows in `data`.
#'
#' @examples
#' group_by_dose(nm_busulfan)
#'
#' @export
group_by_dose <- function(
  data,
  dictionary = list("ID" = "ID", "EVID" = "EVID", "TIME" = "TIME"),
  ...
) {
  data |>
    dplyr::group_by(dplyr::pick(dictionary$ID)) |>
    dplyr::mutate(`_dose_idx` = cumsum(.data[[dictionary$EVID]])) |>
    dplyr::group_by(dplyr::pick(dictionary$ID, "_dose_idx")) |>
    dplyr::mutate(`_has_sample` = any(.data[[dictionary$EVID]] == 0)) |>
    dplyr::group_by(dplyr::pick(dictionary$ID)) |>
    dplyr::mutate(
      `_group` = cumsum(.data[[dictionary$EVID]] == 1 & .data$`_has_sample`)
    ) |>
    dplyr::pull(.data$`_group`)
}
