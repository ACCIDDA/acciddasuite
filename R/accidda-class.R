#' Constructors for the pipeline's typed S3 objects
#'
#' Low-level constructors that assemble and type-check the four pipeline
#' objects. \code{accidda_data} / \code{accidda_ncast} carry the full metadata
#' backbone (\code{key}, \code{target}, \code{window}, \code{interval},
#' \code{history}); \code{accidda_cv} / \code{accidda_fcast} keep only
#' \code{key}, \code{target} and \code{interval}. User-facing validation lives
#' in \code{\link{check_data}}.
#'
#' @name accidda-class
#' @keywords internal
#' @noRd
NULL


#' @keywords internal
#' @noRd
new_accidda_data <- function(data, key, target, window, interval, history) {
  stopifnot(
    is.data.frame(data),
    is.character(key), length(key) > 0L, all(key %in% names(data)),
    is.character(target), length(target) == 1L,
    inherits(window, "Date"), length(window) == 2L,
    is.numeric(interval), length(interval) == 1L,
    is.logical(history), length(history) == 1L
  )
  structure(
    list(
      data = data,
      key = key,
      target = target,
      window = window,
      interval = as.integer(interval),
      history = history
    ),
    class = "accidda_data"
  )
}


#' @keywords internal
#' @noRd
new_accidda_ncast <- function(
  data,
  key,
  target,
  window,
  interval,
  history,
  meta
) {
  stopifnot(is.list(meta))
  # Same backbone as accidda_data, validated in one place, plus `meta`.
  out <- new_accidda_data(data, key, target, window, interval, history)
  out$meta <- meta
  class(out) <- "accidda_ncast"
  out
}


#' @keywords internal
#' @noRd
new_accidda_cv <- function(forecasts, oracle, score, models, meta, data) {
  stopifnot(is.list(models), is.list(meta), is.data.frame(data))
  structure(
    list(
      forecasts = forecasts,
      oracle = oracle,
      score = score,
      models = models,
      meta = meta,
      data = data
    ),
    class = "accidda_cv"
  )
}


#' @keywords internal
#' @noRd
new_accidda_fcast <- function(hub, score, meta) {
  stopifnot(is.list(hub), is.list(meta))
  structure(
    list(hub = hub, score = score, meta = meta),
    class = "accidda_fcast"
  )
}


#' Read the shared metadata backbone from a pipeline object
#'
#' Returns the fields shared by every stage (\code{key}, \code{target},
#' \code{interval}), plus \code{window} and \code{history} for
#' \code{accidda_data} / \code{accidda_ncast}.
#' @param x An \code{accidda_data}, \code{accidda_ncast}, \code{accidda_cv} or
#'   \code{accidda_fcast}.
#' @return A named list.
#' @keywords internal
#' @noRd
accidda_meta <- function(x) {
  if (inherits(x, c("accidda_data", "accidda_ncast"))) {
    x[c("key", "target", "window", "interval", "history")]
  } else if (inherits(x, c("accidda_cv", "accidda_fcast"))) {
    x$meta[c("key", "target", "interval")]
  } else {
    stop(
      "`x` must be an accidda_data, accidda_ncast, accidda_cv or ",
      "accidda_fcast object.\n",
      "Run check_data() on your data frame first."
    )
  }
}
