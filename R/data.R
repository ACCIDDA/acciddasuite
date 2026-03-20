#' Weekly COVID-19 hospital admissions for New York
#'
#' Weekly confirmed COVID-19 hospital admissions for New York state
#' from the CDC NHSN, fetched via \code{\link{get_data}} with revision
#' history. Covers August 2020 through March 2026.
#'
#' Pass this data frame through \code{\link{check_data}} to use it in
#' the acciddasuite pipeline.
#'
#' @format A data frame with 292 rows and 5 columns:
#' \describe{
#'   \item{as_of}{Date the observation was reported.}
#'   \item{location}{State abbreviation (\code{"NY"}).}
#'   \item{target}{Forecast target (\code{"wk inc covid hosp"}).}
#'   \item{target_end_date}{End date of the epiweek.}
#'   \item{observation}{Confirmed hospital admissions count.}
#' }
#'
#' @source CDC NHSN via \code{\link[epidatr]{pub_covidcast}}.
#'
#' @examples
#' \dontrun{
#' example_data |> check_data() |> get_fcast(eval_start_date = "2025-01-01")
#' }
"example_data"
