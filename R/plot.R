#' Plot method for a `run_eval()` object
#'
#' @description
#' One plot `type` is currently available:
#'
#' - `type = "vpc"` plots visual predictive checks (VPC) with [vpc::vpc()].
#'
#' @param x An object.
#' @param type Character string, indicating the type of plot. Options are `"vpc"`.
#' @param ... Arguments passed to or from other methods.
#'
#' @returns A plot.
#' @export
plot.mipdeval_results <- function(x, type = "vpc", ...) {
  type <- rlang::arg_match(type)
  plot_fun <- switch (
    type,
    vpc = plot_vpc
  )
  plot_fun(x, ...)
}

plot_vpc <- function(res, ...) {
  rlang::check_installed("vpc", reason = "for VPC plotting.")
  rlang::check_dots_used()
  vpc::vpc(
    sim = res$sim,
    obs = dplyr::filter(res$data, .data$EVID == 0),
    ...
  )
}
