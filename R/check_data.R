#' Validate surveillance data for the acciddasuite pipeline
#'
#' Checks that a data frame has the required columns and structure for
#' use with \code{\link{get_ncast}} and \code{\link{get_fcast}}.
#' Returns a typed \code{accidda_data} object that downstream functions
#' can accept without repeating validation.
#'
#' @param df A data frame (or tibble) with at least:
#'   \code{target_end_date} (Date), \code{observation} (numeric),
#'   \code{location} (character), and \code{target} (character).
#'   An optional \code{as_of} (Date) column enables nowcasting via
#'   \code{\link{get_ncast}}.
#'
#' @return An \code{accidda_data} object (a list) with:
#'   \describe{
#'     \item{data}{The validated data frame with coerced date types.}
#'     \item{location}{Single location identifier.}
#'     \item{target}{Single target identifier.}
#'     \item{window}{Named vector with \code{from} and \code{to} dates.}
#'     \item{history}{Logical. \code{TRUE} if revision history
#'       (\code{as_of}) is present.}
#'   }
#'
#' @examples
#' \dontrun{
#' # From get_data
#' df <- get_data("covid", "ny") |> check_data()
#'
#' # User-provided data
#' my_df <- read.csv("my_data.csv") |> check_data()
#'
#' # Then into the pipeline
#' df |> get_fcast(eval_start_date = "2025-01-01")
#' }
#'
#' @export
check_data <- function(df) {
  # Already validated: return as-is
  if (inherits(df, "accidda_data")) return(df)

  # --- Column checks ---
  if (!is.data.frame(df)) stop("`df` must be a data frame.")

  required <- c("target_end_date", "observation", "location", "target")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # --- Type coercion ---
  df$target_end_date <- as.Date(df$target_end_date)
  df$observation <- as.numeric(df$observation)
  df$location <- as.character(df$location)
  df$target <- as.character(df$target)

  if (any(is.na(df$target_end_date))) {
    stop("`target_end_date` contains values that cannot be coerced to Date.")
  }

  # --- Single location / target ---
  locations <- unique(df$location)
  targets <- unique(df$target)

  if (length(locations) != 1) {
    stop(
      "Data must contain exactly one location (found ",
      length(locations), ": ",
      paste(head(locations, 5), collapse = ", "),
      if (length(locations) > 5) ", ..." else "",
      "). Filter before calling check_data()."
    )
  }
  if (length(targets) != 1) {
    stop(
      "Data must contain exactly one target (found ",
      length(targets), ": ",
      paste(head(targets, 5), collapse = ", "),
      if (length(targets) > 5) ", ..." else "",
      "). Filter before calling check_data()."
    )
  }

  # --- Revision history ---
  history <- "as_of" %in% names(df) && length(unique(df$as_of)) > 1
  if (history) {
    df$as_of <- as.Date(df$as_of)
  }

  # --- Window ---
  window <- c(
    from = min(df$target_end_date),
    to   = max(df$target_end_date)
  )

  result <- list(
    data     = df,
    location = locations,
    target   = targets,
    window   = window,
    history  = history
  )
  class(result) <- "accidda_data"
  result
}


#' @export
print.accidda_data <- function(x, ...) {
  cat("<accidda_data>\n\n")
  cat("Location:", x$location, "\n")
  cat("Target:  ", x$target, "\n")
  cat("Window:  ", as.character(x$window["from"]),
      "to", as.character(x$window["to"]),
      "(", nrow(x$data[!duplicated(x$data$target_end_date), ]), "dates )\n")
  if (x$history) {
    as_of_rng <- range(x$data$as_of)
    cat("History:  TRUE (",
        as.character(as_of_rng[1]), "to", as.character(as_of_rng[2]), ")\n")
  } else {
    cat("History:  FALSE\n")
  }
  invisible(x)
}
