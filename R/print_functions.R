#' Print results from run_eval()
#'
#' @param x output object from `run_eval()`.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.mipdeval_results <- function(x, ...) {
  print(x$stats_summ, ...)
  print(x$shrinkage, ...)
  print(x$bayesian_impact, ...)
}

#' Print predictive performance statistics from run_eval()
#'
#' @inheritParams print.mipdeval_results
#' @param x stats summary object in output from `run_eval()`.
#'
#' @export
print.mipdeval_results_stats_summ <- function(x, ...) {
  x |>
    dplyr::mutate(
      type = dplyr::case_when(
        type == "iter_ipred" ~ "Iterative forecasting",
        type == "map_ipred" ~ "Retrospective MAP Bayesian",
        type == "pred" ~ "Population prediction"
      )
    ) |>
    knitr::kable(format = "simple", caption = "Predictive performance") |>
    print(...)
}

#' Print shrinkage results from run_eval()
#'
#' @inheritParams print.mipdeval_results
#' @param x shrinkage object in output from `run_eval()`.
#'
#' @export
print.mipdeval_results_shrinkage <- function(x, ...) {
  x |>
    dplyr::rename("Iteration" = .data$`_iteration`) |>
    knitr::kable(format = "simple", caption = "Shrinkage (%)") |>
    print(...)
}

#' Print Bayesian impact results from run_eval()
#'
#' @inheritParams print.mipdeval_results
#' @param x bayesian impact object in output from `run_eval()`.
#'
#' @export
print.mipdeval_results_bayesian_impact <- function(x, ...) {
  x |>
    rlang::set_names(gsub("bi_", "", names(x))) |>
    knitr::kable(format = "simple", caption = "Bayesian impact (%), based on RMSE / MAPE") |>
    print(...)
}
