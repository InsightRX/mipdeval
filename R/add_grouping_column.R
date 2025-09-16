#' Add grouping based on time bins
#'
#' @inheritParams run_eval
#'
#' @param fun grouping function to use. Can be custom function, or one of the
#' built-in grouping functions: group_by_time, group_by_dose. If a custom
#' function, input to the function needs to take at least the arguments `.data`
#' , `dictionary` and `...`, and return a data.frame with the same number of
#' rows as the input (and of course a new column providing the grouping).
#' @param label column name of grouper column in dataset
#' @param ... passed onto the chosen grouper function, see the respective
#' grouping functions for more details. E.g. for `group_by_time`, need a `bins`
#' argument.
#'
#' @returns data.frame
#'
#' @export
add_grouping_column <- function(
    data,
    fun = group_by_time,
    label = "group",
    ...
) {
  if(label %in% names(data)) {
    cli::cli_alert_warning("A `group` column already existed in `data`, overwriting.")
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
#' @param bins vector of bin separators. Suggestion to keep the last bin
#' separator `Inf`, to ensure all points are included in a group.
#'
#' @returns a vector of the same length as the number of rows in `.data`
group_by_time <- function(
    data,
    bins = c(0, 24, 48, 72, 96, Inf),
    dictionary = list(
      "TIME" = "TIME"
    ),
    ...
) {
  cut(
    data[[dictionary$TIME]],
    breaks = bins,
    include.lowest = TRUE
  ) |>
    as.numeric()
}

#' Will create a separate group for each dose intervals that contains at least
#' one sample
#'
#' @inheritParams add_grouping_column
#'
#' @returns a vector of the same length as the number of rows in `.data`
group_by_dose <- function(
    data,
    dictionary = list(
      "ID" = "ID",
      "EVID" = "EVID",
      "TIME" = "TIME"
    ),
    ...
) {
  data |>
    dplyr::group_by_at(dictionary$ID) |>
    dplyr::mutate(`_dose_idx` = cumsum(.data[[dictionary$EVID]])) |>
    dplyr::group_by_at(c(dictionary$ID, "_dose_idx")) |>
    dplyr::mutate(`_has_sample` = any(.data[[dictionary$EVID]] == 0)) |>
    dplyr::group_by_at(dictionary$ID) |>
    dplyr::mutate(`_group` = cumsum(.data[[dictionary$EVID]] == 1 & .data$`_has_sample`)) |>
    dplyr::pull(`_group`)
}
