#' Shared print helpers
#'
#' Internal helpers for printing \code{accidda} objects using a consistent
#' summary format.
#'
#' @keywords internal
#' @noRd
cat_field <- function(label, ...) {
  cat(sprintf("%-9s %s\n", paste0(label, ":"), paste0(...)))
}


#' Format series information
#'
#' Create a summary of the number of series and their key columns.
#'
#' @keywords internal
#' @noRd
fmt_series <- function(df, key) {
  sprintf("%d (%s)", nrow(unique(df[key])), paste(key, collapse = " x "))
}


#' Format a date range and reporting interval
#'
#' @keywords internal
#' @noRd
fmt_window <- function(from, to, interval) {
  paste0(from, " to ", to, " (", interval, "-day interval)")
}


#' Print an \code{accidda_data} object
#'
#' @param x An \code{accidda_data} object.
#' @param ... Ignored.
#'
#' @export
print.accidda_data <- function(x, ...) {
  m <- accidda_meta(x)
  cat("<accidda_data>\n")
  cat_field("Target", m$target)
  cat_field("Series", fmt_series(x$data, m$key))
  cat_field("Window", fmt_window(m$window[["from"]], m$window[["to"]], m$interval))
  if (m$history) {
    cat_field("History", min(x$data$as_of), " to ", max(x$data$as_of))
  }
  invisible(x)
}


#' Print an \code{accidda_ncast} object
#'
#' Display a summary of the target, series, data window, and nowcast period.
#'
#' @param x An \code{accidda_ncast} object.
#' @param ... Ignored.
#'
#' @export
print.accidda_ncast <- function(x, ...) {
  m <- accidda_meta(x)
  cat("<accidda_ncast>\n")
  cat_field("Target", m$target)
  cat_field("Series", fmt_series(x$data, m$key))
  cat_field("Window", fmt_window(m$window[["from"]], m$window[["to"]], m$interval))
  corrected <- x$data$target_end_date[!is.na(x$data$ncast_lower)]
  if (length(corrected) > 0) {
    cat_field("Nowcast", min(corrected), " to ", max(corrected))
  }
  invisible(x)
}


#' Print an \code{accidda_cv} object
#'
#' Display a summary of the target, series, data window, and
#' cross-validation settings.
#'
#' @param x An \code{accidda_cv} object.
#' @param ... Ignored.
#'
#' @export
print.accidda_cv <- function(x, ...) {
  m <- accidda_meta(x)
  cat("<accidda_cv>\n")
  cat_field("Target", m$target)
  cat_field("Series", fmt_series(x$data, m$key))
  cat_field(
    "Window",
    fmt_window(min(x$data$target_end_date), max(x$data$target_end_date), m$interval)
  )
  cat_field(
    "CV",
    length(x$models), " models x ",
    dplyr::n_distinct(x$forecasts$reference_date), " origins (h = ", x$meta$h, ")"
  )
  invisible(x)
}


#' Print an \code{accidda_fcast} object
#'
#' Display a summary of the target, series, forecast period, and models used.
#'
#' @param x An \code{accidda_fcast} object.
#' @param ... Ignored.
#'
#' @export
print.accidda_fcast <- function(x, ...) {
  m <- accidda_meta(x)
  rng <- range(x$hub$model_out_tbl$target_end_date)
  cat("<accidda_fcast>\n")
  cat_field("Target", m$target)
  cat_field("Series", fmt_series(x$hub$model_out_tbl, m$key))
  cat_field("Forecast", rng[1], " to ", rng[2], " (h = ", x$meta$h, ")")
  # Distinct models in the forecast itself; ENSEMBLE is always one of them.
  n_models <- dplyr::n_distinct(x$hub$model_out_tbl$model_id) - 1L
  cat_field("Models", n_models, " + ENSEMBLE")
  invisible(x)
}
