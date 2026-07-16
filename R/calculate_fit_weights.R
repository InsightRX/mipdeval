#' Construct a fit-weighting scheme specification
#'
#' Self-documenting helper that builds a `fit_weights` object describing how
#' older observations should be downweighted relative to more recent ones
#' during the iterative MAP Bayesian fitting step. The result can be passed as
#' the `weights` argument to [run_eval()].
#'
#' Available schemes:
#' - `"weight_all"`: all samples weighted equally (weight = 1).
#' - `"weight_last_only"`: only the most recent sample is used (weight = 1),
#'   all others are excluded (weight = 0).
#' - `"weight_last_two_only"`: only the two most recent samples are used.
#' - `"weight_gradient_linear"`: weights increase linearly from a minimum
#'   (`w1`) for samples older than `t1` days to a maximum (`w2`) for samples
#'   more recent than `t2` days. Accepts scheme parameter `gradient`, a list
#'   with named elements `t1`, `w1`, `t2`, `w2`. Default:
#'   `list(t1 = 7, w1 = 0, t2 = 2, w2 = 1)`.
#' - `"weight_gradient_exponential"`: weights decay exponentially with the age
#'   of the sample. Accepts scheme parameters `t12_decay` (half-life of decay
#'   in hours, default 48) and `t_start` (delay in hours before decay starts,
#'   default 0).
#'
#' @param scheme name of the weighting scheme (see Details).
#' @param ... scheme-specific parameters, e.g. `t12_decay = 72` for
#'   `"weight_gradient_exponential"`, or
#'   `gradient = list(t1 = 5, w1 = 0.1, t2 = 1, w2 = 1)` for
#'   `"weight_gradient_linear"`.
#'
#' @returns an object of class `fit_weights`.
#' @examples
#' fit_weights("weight_all")
#' fit_weights("weight_gradient_exponential", t12_decay = 72)
#' fit_weights("weight_gradient_linear", gradient = list(t1 = 5, w1 = 0.1, t2 = 1, w2 = 1))
#' @export
fit_weights <- function(
  scheme = c(
    "weight_all",
    "weight_last_only",
    "weight_last_two_only",
    "weight_gradient_linear",
    "weight_gradient_exponential"
  ),
  ...
) {
  scheme <- match.arg(scheme)
  structure(
    list(scheme = scheme, params = list(...)),
    class = "fit_weights"
  )
}

#' Calculate time-based sample weights for MAP Bayesian fitting
#'
#' Downweights older observations relative to more recent ones during the
#' iterative MAP Bayesian fitting step. Can be passed as the `weights`
#' argument to [run_eval()].
#'
#' `weights` may be a [fit_weights()] object, a string naming a scheme, or a
#' named list with a `scheme` element plus optional scheme-specific parameters
#' (e.g. `list(scheme = "weight_gradient_exponential", t12_decay = 72)`). See
#' [fit_weights()] for the available schemes and their parameters.
#'
#' @param weights weighting scheme: a [fit_weights()] object, a string with the
#'   scheme name, or a named list with a `scheme` element plus optional
#'   scheme-specific parameters.
#' @param t numeric vector of observation times (in hours)
#'
#' @returns numeric vector of weights the same length as `t`, or `NULL` if
#'   `weights` is `NULL` or the scheme is not recognized.
#' @export
calculate_fit_weights <- function(weights = NULL, t = NULL) {
  if (is.null(weights) || is.null(t)) return(NULL)

  weights <- as_fit_weights(weights)
  if (is.null(weights)) return(NULL)

  weight_vec <- switch(
    weights$scheme,
    weight_gradient_linear      = .wt_gradient_linear(t, weights$params),
    weight_gradient_exponential = .wt_gradient_exponential(t, weights$params),
    weight_last_only            = .wt_last_only(t),
    weight_last_two_only        = .wt_last_two_only(t),
    weight_all                  = .wt_all(t)
  )

  if (!is.null(weight_vec)) {
    weight_vec[t < 0] <- 0
  }

  weight_vec
}

# Normalize the various accepted `weights` inputs into a `fit_weights` object.
# Returns NULL (with a warning) when the scheme cannot be recognized, so that
# callers can cleanly ignore invalid input rather than error.
as_fit_weights <- function(weights) {
  if (inherits(weights, "fit_weights")) return(weights)

  if (is.character(weights)) {
    scheme <- weights
    params <- list()
  } else if (is.list(weights)) {
    scheme <- weights$scheme
    params <- weights[setdiff(names(weights), "scheme")]
  } else {
    warning("Weighting scheme not recognized, ignoring weights.")
    return(NULL)
  }

  valid_schemes <- c(
    "weight_gradient_linear",
    "weight_gradient_exponential",
    "weight_last_only",
    "weight_last_two_only",
    "weight_all"
  )

  if (length(scheme) != 1 || is.na(scheme) || !scheme %in% valid_schemes) {
    warning("Weighting scheme not recognized, ignoring weights.")
    return(NULL)
  }

  structure(list(scheme = scheme, params = params), class = "fit_weights")
}

.wt_gradient_linear <- function(t, params = list()) {
  gradient <- list(t1 = 7, w1 = 0, t2 = 2, w2 = 1)
  if (!is.null(params$gradient)) {
    gradient[names(params$gradient)] <- params$gradient
  }
  if (gradient$t2 > gradient$t1) {
    warning(
      "weight_gradient_linear: t2 (", gradient$t2, ") > t1 (", gradient$t1,
      "). t1 should be the older threshold and t2 the more recent one."
    )
  }
  t_start <- max(c(0, max(t) - gradient$t1 * 24))
  t_end   <- max(c(0, max(t) - gradient$t2 * 24))
  if (t_end <= t_start) {
    ifelse(t >= t_end, gradient$w2, gradient$w1)
  } else {
    ifelse(
      t <= t_start, gradient$w1,
      ifelse(
        t >= t_end, gradient$w2,
        gradient$w1 + (gradient$w2 - gradient$w1) * (t - t_start) / (t_end - t_start)
      )
    )
  }
}

.wt_gradient_exponential <- function(t, params = list()) {
  t12_decay <- if (!is.null(params$t12_decay)) params$t12_decay else 48
  k_decay <- log(2) / t12_decay
  t_diff <- max(t) - t
  if (!is.null(params$t_start)) {
    t_diff <- t_diff - params$t_start
    t_diff <- ifelse(t_diff < 0, 0, t_diff)
  }
  exp(-k_decay * t_diff)
}

.wt_last_only <- function(t) {
  weight_vec <- rep(0, length(t))
  weight_vec[which.max(t)] <- 1
  weight_vec
}

.wt_last_two_only <- function(t) {
  weight_vec <- rep(0, length(t))
  ranked <- order(t, decreasing = TRUE)
  weight_vec[ranked[1]] <- 1
  if (length(t) > 1) weight_vec[ranked[2]] <- 1
  weight_vec
}

.wt_all <- function(t) {
  rep(1, length(t))
}
