#' Internal shared helpers
#' @name acciddasuite-utils
#' @keywords internal
#' @noRd
NULL


#' Detect the reporting interval in days (7 = weekly)
#'
#' Modal day-spacing between consecutive distinct \code{target_end_date}s,
#' recorded by \code{\link{check_data}}.
#' @param dates A Date vector (duplicates allowed).
#' @return A single positive integer (days).
#' @keywords internal
#' @noRd
detect_interval <- function(dates) {
  u <- sort(unique(dates))
  if (length(u) < 2L) {
    stop(
      "Need at least two distinct `target_end_date` values to determine ",
      "the reporting interval."
    )
  }
  diffs <- as.integer(diff(u))
  tab <- table(diffs)
  interval <- as.integer(names(tab)[which.max(tab)])
  if (interval <= 0L) {
    stop("Could not determine a positive reporting interval from the dates.")
  }
  interval
}


#' Forecast-ready data frame from a typed object
#'
#' Returns the data frame, keeping the latest revision per
#' \code{target_end_date} and location when revision history is present.
#' @param x An \code{accidda_data} or \code{accidda_ncast} object.
#' @return A data frame, one row per \code{target_end_date}.
#' @keywords internal
#' @noRd
extract_series <- function(x) {
  if (inherits(x, "accidda_ncast") || inherits(x, "accidda_data")) {
    df <- x$data
  } else {
    stop(
      "`x` must be an accidda_data or accidda_ncast object.\n",
      "Run check_data() on your data frame first."
    )
  }

  if ("as_of" %in% names(df)) {
    df <- df |>
      dplyr::group_by(location, target_end_date) |>
      dplyr::filter(as_of == max(as_of)) |>
      dplyr::ungroup() |>
      dplyr::select(-as_of)
  }
  df
}


#' Build the regular tsibble that models are fitted on
#'
#' A \code{target_end_date} / \code{observation} tsibble with missing rows
#' dropped and implicit gaps filled.
#' @param df A forecast-ready data frame with an \code{observation} column.
#' @return A regular tsibble indexed by \code{target_end_date}.
#' @keywords internal
#' @noRd
#' @importFrom dplyr filter select
#' @importFrom tsibble as_tsibble fill_gaps
as_model_ts <- function(df) {
  df |>
    dplyr::filter(!is.na(observation)) |>
    dplyr::select(target_end_date, observation) |>
    tsibble::as_tsibble(index = target_end_date) |>
    tsibble::fill_gaps()
}


#' Truncate a distribution to \code{[0, Inf)} (counts are non-negative)
#' @param dist A \code{distributional} vector.
#' @return The truncated distribution.
#' @keywords internal
#' @noRd
#' @importFrom distributional dist_truncated
truncate_counts <- function(dist) {
  distributional::dist_truncated(dist, lower = 0, upper = Inf)
}


#' Equal-weight mixture of distributions
#' @param dists A \code{distributional} vector (or list) of distributions.
#' @return A single mixture distribution.
#' @keywords internal
#' @noRd
#' @importFrom distributional dist_mixture
mix_equally <- function(dists) {
  dists <- as.list(dists)
  n <- length(dists)
  do.call(
    distributional::dist_mixture,
    c(dists, list(weights = rep(1 / n, n)))
  )
}


#' Error unless \code{models} is a non-empty, uniquely named list
#' @param models A named list of fable model definitions.
#' @return \code{models}, invisibly.
#' @keywords internal
#' @noRd
validate_models <- function(models) {
  if (!is.list(models) || length(models) == 0L) {
    stop("`models` must be a non-empty list of fable model definitions.")
  }
  nms <- names(models)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms) > 0L) {
    stop(
      "`models` must be a uniquely named list; each name labels a model.\n",
      "e.g. list(ETS = fable::ETS(observation), ARIMA = fable::ARIMA(observation))"
    )
  }
  invisible(models)
}
