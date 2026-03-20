#' Get hospitalisation data
#'
#' Fetch confirmed US hospital admissions from the NHSN source
#' for COVID-19, influenza, or RSV using \link[epidatr]{pub_covidcast}.
#'
#' By default the latest version of each observation is returned. Set
#' \code{revisions = TRUE} to retrieve all available revision history,
#' which is needed by \code{\link{get_ncast}}.
#'
#' @param pathogen Character. One of "covid", "flu" or "rsv".
#' @param geo_value Character vector. Geographic value as per
#'   \link[epidatr]{pub_covidcast} to filter by.
#' @param revisions Logical. If \code{TRUE}, fetches all available revision
#'   history, producing data ready for \code{\link{get_ncast}}.
#'   Default is \code{FALSE} (latest version only).
#'
#' @return An \code{accidda_data} object (see \code{\link{check_data}}).
#'
#' @seealso \link[epidatr]{pub_covidcast}
#' @seealso \link[hubverse]{https://docs.hubverse.io/en/stable/user-guide/target-data.html}
#' @export
#' @importFrom epidatr pub_covidcast
#' @importFrom dplyr transmute
#' @examples
#' get_data(pathogen = "covid", geo_value = "ny")
#'
#' # Fetch revision history for nowcasting
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
