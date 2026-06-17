#' Constructors for the pipeline's typed S3 objects
#'
#' Low-level constructors that assemble and type-check the four pipeline
#' objects, which share a metadata backbone (\code{location}, \code{target},
#' \code{window}, \code{interval}, \code{history}). User-facing validation
#' lives in \code{\link{check_data}}.
#'
#' @name accidda-class
#' @keywords internal
#' @noRd
NULL


#' @keywords internal
#' @noRd
new_accidda_data <- function(data, location, target, window, interval, history) {
  stopifnot(
    is.data.frame(data),
    is.character(location), length(location) == 1L,
    is.character(target), length(target) == 1L,
    inherits(window, "Date"), length(window) == 2L,
    is.numeric(interval), length(interval) == 1L,
    is.logical(history), length(history) == 1L
  )
  structure(
    list(
      data = data,
      location = location,
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
  location,
  target,
  window,
  interval,
  history,
  plot
) {
  stopifnot(
    is.data.frame(data),
    is.character(location), length(location) == 1L,
    is.character(target), length(target) == 1L,
    inherits(plot, "ggplot")
  )
  structure(
    list(
      data = data,
      location = location,
      target = target,
      window = window,
      interval = as.integer(interval),
      history = history,
      plot = plot
    ),
    class = "accidda_ncast"
  )
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


#' Read the shared metadata backbone from any pipeline object
#'
#' Returns \code{location}, \code{target}, \code{interval} (plus \code{window}
#' and \code{history} for dataset objects), whichever stage produced \code{x}.
#' @param x An \code{accidda_data}, \code{accidda_ncast} or \code{accidda_cv}.
#' @return A named list.
#' @keywords internal
#' @noRd
accidda_meta <- function(x) {
  if (inherits(x, "accidda_cv")) {
    return(x$meta)
  }
  if (inherits(x, c("accidda_data", "accidda_ncast"))) {
    return(list(
      location = x$location,
      target = x$target,
      window = x$window,
      interval = x$interval,
      history = x$history
    ))
  }
  stop(
    "`x` must be an accidda_data, accidda_ncast or accidda_cv object.\n",
    "Run check_data() on your data frame first."
  )
}
