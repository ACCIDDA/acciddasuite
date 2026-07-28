#' Weekly influenza hospital admissions for New York and California
#'
#' Weekly confirmed influenza hospital admissions for New York and California
#' (CDC NHSN), with revision history, fetched via \code{\link{get_data}};
#' pass through \code{\link{check_data}} to use it.
#'
#' The archive is pinned to the 14 December 2025 history, so the most recent weeks 
#' are still right-truncated and \code{\link{get_ncast}} has reporting delay to model.
#' To regenerate with \code{data-raw/example_data.R}.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{as_of}{Date the observation was reported.}
#'   \item{location}{State abbreviation (\code{"NY"} or \code{"CA"}).}

#'   \item{target}{Forecast target (\code{"wk inc flu hosp"}).}
#'   \item{target_end_date}{End date of the epiweek.}
#'   \item{observation}{Confirmed hospital admissions count.}
#' }
#'
#' @source CDC NHSN via \code{\link[epidatr]{pub_covidcast}}.
#'
#' @examples
#' \dontrun{
#' example_data |> check_data()
#' }
"example_data"
