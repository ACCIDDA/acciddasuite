#' Validate surveillance data
#'
#' Validates a surveillance data frame and returns a typed \code{accidda_data}
#' object that the rest of the pipeline accepts without re-validating.
#'
#' @param data A data frame with \code{target_end_date} (Date),
#'   \code{observation} (numeric), \code{location} (character) and
#'   \code{target} (character). An optional \code{as_of} (Date) column enables
#'   nowcasting (\code{\link{get_ncast}}). An existing \code{accidda_data} is
#'   returned unchanged.
#'
#' @return An \code{accidda_data} object:
#'   \describe{
#'     \item{data}{Validated data frame with coerced types.}
#'     \item{location, target}{Single location / target identifier.}
#'     \item{window}{Named \code{from} / \code{to} dates.}
#'     \item{interval}{Reporting interval in days (7 = weekly).}
#'     \item{history}{\code{TRUE} if revision history (\code{as_of}) is present.}
#'   }
#'
#' @examples
#' \dontrun{
#' x <- get_data("covid", "ny") |> check_data()
#' my_x <- read.csv("my_data.csv") |> check_data()
#' }
#'
#' @export
check_data <- function(data) {
  # Already validated: return as-is
  if (inherits(data, "accidda_data")) {
    return(data)
  }

  # --- Column checks ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  required <- c("target_end_date", "observation", "location", "target")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # --- Type coercion ---
  data$target_end_date <- as.Date(data$target_end_date)
  data$observation <- as.numeric(data$observation)
  data$location <- as.character(data$location)
  data$target <- as.character(data$target)

  if (any(is.na(data$target_end_date))) {
    stop("`target_end_date` contains values that cannot be coerced to Date.")
  }

  # --- Single location / target ---
  locations <- unique(data$location)
  targets <- unique(data$target)

  if (length(locations) != 1) {
    stop(
      "Data must contain exactly one location (found ",
      length(locations),
      ": ",
      paste(head(locations, 5), collapse = ", "),
      if (length(locations) > 5) ", ..." else "",
      "). Filter before calling check_data()."
    )
  }
  if (length(targets) != 1) {
    stop(
      "Data must contain exactly one target (found ",
      length(targets),
      ": ",
      paste(head(targets, 5), collapse = ", "),
      if (length(targets) > 5) ", ..." else "",
      "). Filter before calling check_data()."
    )
  }

  # --- Revision history ---
  history <- "as_of" %in% names(data) && length(unique(data$as_of)) > 1
  if (history) {
    data$as_of <- as.Date(data$as_of)
  }

  # --- Reporting interval (time unit) and window ---
  # Detected once here so downstream stages derive their time-based constants
  # (e.g. the 1-year minimum in get_cv, the weekly horizon in fable_to_hub)
  # instead of hard-coding them.
  interval <- detect_interval(data$target_end_date)
  window <- c(
    from = min(data$target_end_date),
    to = max(data$target_end_date)
  )

  new_accidda_data(
    data = data,
    location = locations,
    target = targets,
    window = window,
    interval = interval,
    history = history
  )
}
