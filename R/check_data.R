#' Validate surveillance data
#'
#' Validate and standardise surveillance data for use throughout the package.
#' The returned \code{incast_data} object can be passed directly to forecasting
#' and nowcasting functions.
#'
#' Data must contain one row per time series and reporting date (and
#' \code{as_of}, if present). All series must have the same reporting interval,
#' share the same reporting dates, and end on the same date. Series may begin
#' at different times and may contain missing reporting periods.
#'
#' @author Cyril Geismar
#' 
#' @param data A data frame containing \code{target_end_date} (\code{Date}),
#' \code{observation} (numeric), \code{target} (character), and one or more
#' key columns. An optional \code{as_of} (\code{Date}) column enables
#' nowcasting with \code{\link{get_ncast}}. If \code{data} is already an
#' \code{incast_data} object, it is returned unchanged.
#'
#' @param key Character vector giving the column name(s) that uniquely identify
#' each time series, equivalent to the key of a
#' \code{\link[tsibble]{tsibble}}. Each unique combination of key values is
#' treated as a separate series. Defaults to \code{"location"}.
#'
#' @return An \code{incast_data} object containing:
#' \describe{
#' \item{data}{Validated data with standardised column types.}
#' \item{key}{Names of the key columns.}
#' \item{target}{Target variable name.}
#' \item{window}{Start and end dates of the data.}
#' \item{interval}{Reporting interval in days (for example, 7 for weekly data).}
#' \item{history}{Logical indicating whether revision history (\code{as_of}) is available.}
#' }
#'
#' @examples
#' \dontrun{
#' x <- get_data("covid", c("ny", "ca")) |> check_data()
#' my_x <- read.csv("my_data.csv") |>
#'   check_data(key = c("location", "age_group"))
#' }
#'
#' @export
check_data <- function(data, key = "location") {
  if (!is.character(key) || length(key) == 0L || anyNA(key)) {
    stop("`key` must be a character vector of column names.")
  }

  # Already validated: return as-is
  if (inherits(data, "incast_data")) {
    if (!missing(key) && !identical(key, data$key)) {
      stop(
        "`data` is already validated with key: ",
        paste(data$key, collapse = ", ")
      )
    }
    return(data)
  }

  # --- Column checks ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  required <- c("target_end_date", "observation", "target", key)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- Type coercion ---
  data$target_end_date <- as.Date(data$target_end_date)
  data$observation <- as.numeric(data$observation)
  data$target <- as.character(data$target)
  data[key] <- lapply(data[key], as.character)

  if (any(is.na(data$target_end_date))) {
    stop("`target_end_date` contains values that cannot be coerced to Date.")
  }

  # --- One target ---
  target <- unique(data$target)
  if (length(target) != 1) {
    stop(
      "Data must contain exactly one target (found ",
      length(target),
      ": ",
      paste(head(target, 5), collapse = ", "),
      if (length(target) > 5) ", ..." else "",
      "). Filter before calling check_data()."
    )
  }

  # --- Revision history ---
  history <- "as_of" %in% names(data) && length(unique(data$as_of)) > 1
  if (history) {
    data$as_of <- as.Date(data$as_of)
  }

  # --- One row per series and date ---
  # A column that splits the series further (e.g. age_group outside `key`)
  # would otherwise be aggregated silently downstream.
  id_cols <- c(key, "target_end_date", intersect("as_of", names(data)))
  dup <- data[duplicated(data[id_cols]), id_cols, drop = FALSE]
  if (nrow(dup) > 0) {
    stop(
      "Data has more than one row per ", paste(id_cols, collapse = " + "),
      " (e.g. ",
      paste0(id_cols, " = ", vapply(dup[1L, ], as.character, ""), collapse = ", "),
      ").\nIf a column splits the series further (e.g. age_group), add it to ",
      "`key`; otherwise aggregate before calling check_data()."
    )
  }

  # --- Reporting interval (time unit) ---
  # Every series must share the same interval.
  by_series <- split(data$target_end_date, data[key], drop = TRUE)
  intervals <- vapply(
    names(by_series),
    function(s) {
      tryCatch(
        detect_interval(by_series[[s]]),
        error = function(e) {
          stop("Series ", s, ": ", conditionMessage(e), call. = FALSE)
        }
      )
    },
    integer(1L)
  )
  if (length(unique(intervals)) > 1) {
    usual <- as.integer(names(which.max(table(intervals))))
    odd <- intervals[intervals != usual]
    stop(
      "Series report at different intervals (days): most report every ",
      usual, " days, but ",
      paste0(head(names(odd), 6L), " = ", head(odd, 6L), collapse = ", "),
      if (length(odd) > 6L) ", ..." else "",
      ". Resample or filter before calling check_data()."
    )
  }
  interval <- intervals[[1L]]

  # --- Same reporting dates across series ---
  # All series must share the same reporting calendar. Different reporting
  # dates (for example, weeks ending on different weekdays) would create
  # inconsistent time indices across series.
  pooled_gaps <- as.integer(diff(sort(unique(data$target_end_date))))
  if (any(pooled_gaps %% interval != 0L)) {
    stop(
      "Series report on different dates: every series reports every ",
      interval, " days, but not on the same days ",
      "(e.g. one series on Saturdays, another on Sundays).\n",
      "Align the reporting dates before calling check_data()."
    )
  }

  # --- Same end date across series ---
  # Forecasts and cross-validation require all series to have the same final
  # observation date. Different start dates are allowed: shorter series simply
  # have less historical data available.
  ends <- do.call(c, lapply(by_series, max))
  if (length(unique(ends)) > 1) {
    short <- ends[ends < max(ends)]
    stop(
      "All series must end on the same date (", max(ends), "), but ",
      paste0(head(names(short), 6L), " ends ", head(short, 6L), collapse = ", "),
      if (length(short) > 6L) ", ..." else "",
      ".\nTrim every series to a common end date before calling check_data()."
    )
  }

  window <- c(
    from = min(data$target_end_date),
    to = max(data$target_end_date)
  )

  new_incast_data(
    data = data,
    key = key,
    target = target,
    window = window,
    interval = interval,
    history = history
  )
}
