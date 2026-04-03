#' Calculate time-based sample weights for MAP Bayesian fitting
#'
#' Downweights older observations relative to more recent ones during the
#' iterative MAP Bayesian fitting step. Can be passed as the `weights`
#' argument to [run_eval()].
#'
#' Available schemes:
#' - `"weight_all"`: all samples weighted equally (weight = 1).
#' - `"weight_last_only"`: only the most recent sample is used (weight = 1),
#'   all others are excluded (weight = 0).
#' - `"weight_last_two_only"`: only the two most recent samples are used.
#' - `"weight_gradient_linear"`: weights increase linearly from a minimum
#'   (`w1`) for samples older than `t1` days to a maximum (`w2`) for samples
#'   more recent than `t2` days. Accepts optional list element `gradient` with
#'   named elements `t1`, `w1`, `t2`, `w2`. Default:
#'   `list(t1 = 7, w1 = 0, t2 = 2, w2 = 1)`.
#' - `"weight_gradient_exponential"`: weights decay exponentially with the age
#'   of the sample. Accepts optional list elements `t12_decay` (half-life of
#'   decay in hours, default 48) and `t_start` (delay in hours before decay
#'   starts, default 0).
#'
#' For schemes with additional parameters, pass `weights` as a named list with
#' a `scheme` element plus any scheme-specific elements, e.g.:
#' ```r
#' list(scheme = "weight_gradient_exponential", t12_decay = 72)
#' list(scheme = "weight_gradient_linear", gradient = list(t1 = 5, w1 = 0.1, t2 = 1, w2 = 1))
#' ```
#'
#' @param weights weighting scheme: a string with the scheme name, or a named
#'   list with a `scheme` element plus optional scheme-specific parameters.
#' @param t numeric vector of observation times (in hours)
#'
#' @returns numeric vector of weights the same length as `t`, or `NULL` if
#'   `weights` is `NULL`.
#' @export
calculate_fit_weights <- function(weights = NULL, t = NULL) {
  if (is.null(weights) || is.null(t)) return(NULL)

  scheme <- if (is.list(weights)) weights$scheme else weights

  valid_schemes <- c(
    "weight_gradient_linear",
    "weight_gradient_exponential",
    "weight_last_only",
    "weight_last_two_only",
    "weight_all"
  )

  if (!scheme %in% valid_schemes) {
    warning("Weighting scheme not recognized, ignoring weights.")
    return(NULL)
  }

  weight_vec <- NULL

  if (scheme == "weight_gradient_linear") {
    gradient <- list(t1 = 7, w1 = 0, t2 = 2, w2 = 1)
    if (is.list(weights) && !is.null(weights$gradient)) {
      gradient[names(weights$gradient)] <- weights$gradient
    }
    t_start <- max(c(0, max(t) - gradient$t1 * 24))
    t_end   <- max(c(0, max(t) - gradient$t2 * 24))
    if (t_end <= t_start) {
      weight_vec <- ifelse(t >= t_end, gradient$w2, gradient$w1)
    } else {
      weight_vec <- ifelse(
        t <= t_start, gradient$w1,
        ifelse(
          t >= t_end, gradient$w2,
          gradient$w1 + (gradient$w2 - gradient$w1) * (t - t_start) / (t_end - t_start)
        )
      )
    }
  }

  if (scheme == "weight_gradient_exponential") {
    t12_decay <- if (is.list(weights) && !is.null(weights$t12_decay)) weights$t12_decay else 48
    k_decay <- log(2) / t12_decay
    t_diff <- max(t) - t
    if (is.list(weights) && !is.null(weights$t_start)) {
      t_diff <- t_diff - weights$t_start
      t_diff <- ifelse(t_diff < 0, 0, t_diff)
    }
    weight_vec <- exp(-k_decay * t_diff)
  }

  if (scheme == "weight_last_only") {
    weight_vec <- rep(0, length(t))
    weight_vec[length(t)] <- 1
  }

  if (scheme == "weight_last_two_only") {
    weight_vec <- rep(0, length(t))
    weight_vec[length(t)] <- 1
    if (length(t) > 1) weight_vec[length(t) - 1] <- 1
  }

  if (scheme == "weight_all") {
    weight_vec <- rep(1, length(t))
  }

  if (!is.null(weight_vec)) {
    weight_vec[t < 0] <- 0
  }

  weight_vec
}
