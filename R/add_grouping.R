#' Add grouping based on time bins
#'
#' @inheritParams run_eval
#' @param label column name of grouper column in dataset
#' @param bins vector of bin separators
#'
#' @export
#'
add_grouping_bins <- function(
    data,
    label = "group",
    dictionary = list("time" = "TIME"),
    bins = c(0, 24, 48, 72, Inf)
) {
  if(label %in% names(data)) {
    cli::cli_alert_warning("A `group` column already existed in `data`, overwriting.")
  }
  data[[label]] <- cut(
    data[[dictionary$time]],
    breaks = bins,
    include.lowest = TRUE
  ) |> as.numeric()
  data
}
