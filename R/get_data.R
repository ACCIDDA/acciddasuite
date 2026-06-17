#' Fetch hospitalisation data
#'
#' Fetch confirmed US hospital admissions (COVID-19, influenza or RSV) from
#' NHSN via \link[epidatr]{pub_covidcast}.
#'
#' @param pathogen One of "covid", "flu" or "rsv".
#' @param geo_value Geographic value(s) to fetch, as per
#'   \link[epidatr]{pub_covidcast}.
#' @param revisions Logical. If \code{TRUE}, fetch the full revision history
#'   needed by \code{\link{get_ncast}}. Default \code{FALSE} (latest only).
#'
#' @return An \code{accidda_data} object (see \code{\link{check_data}}).
#'
#' @export
#' @importFrom epidatr pub_covidcast
#' @importFrom dplyr transmute
#' @examples
#' get_data(pathogen = "covid", geo_value = "ny")
#'
#' # Revision history for nowcasting
#' get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)

get_data <- function(pathogen, geo_value, revisions = FALSE) {
  pathogen <- match.arg(pathogen, choices = c("covid", "flu", "rsv"))

  signal_map <- c(
    covid = "confirmed_admissions_covid_ew",
    flu = "confirmed_admissions_flu_ew",
    rsv = "confirmed_admissions_rsv_ew"
  )

  raw <- epidatr::pub_covidcast(
    source = "nhsn",
    signals = signal_map[pathogen],
    geo_type = "state",
    time_type = "week",
    geo_values = geo_value,
    issues = if (revisions) "*" else NULL
  ) |>
    dplyr::transmute(
      as_of = issue,
      location = toupper(geo_value),
      target = paste0("wk inc ", pathogen, " hosp"),
      target_end_date = time_value + 6,
      observation = value
    )

  check_data(raw)
}
