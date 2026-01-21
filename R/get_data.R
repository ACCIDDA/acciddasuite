#' Get hospitalisation data
#'
#' Fetch the most recent confirmed US hospital admissions from the NHSN source
#' for COVID-19, influenza, or RSV using \link[epidatr]{pub_covidcast}.
#'
#' @param pathogen Character. One of "covid", "flu" or "rsv".
#' @param geo_values Character vector. Geographic values as per \link[epidatr]{pub_covidcast} to filter by. Defaults to "*" (all states).
#'
#' @return A tibble in `hubverse` format with columns:
#'   - `as_of`: Date when the data was issued
#'   - `location`: State code
#'   - `target`: Target description (e.g., "wk inc covid hosp")
#'   - `target_end_date`: End date of the target week
#'   - `observation`: Observed hospital admissions
#'
#' @seealso \link[epidatr]{pub_covidcast}
#' @seealso \link[hubvserse]{https://docs.hubverse.io/en/stable/user-guide/target-data.html}
#' @export
#' @importFrom epidatr pub_covidcast
#' @importFrom dplyr transmute
#' @examples
#' get_data(pathogen = "covid", geo_values = c("ca", "ny"))

get_data <- function(pathogen, geo_values = "*") {
  pathogen <- match.arg(pathogen, choices = c("covid", "flu", "rsv"))

  signal_map <- c(
    covid = "confirmed_admissions_covid_ew",
    flu = "confirmed_admissions_flu_ew",
    rsv = "confirmed_admissions_rsv_ew"
  )

  epidatr::pub_covidcast(
    source = "nhsn",
    signals = signal_map[pathogen],
    geo_type = "state",
    time_type = "week",
    geo_values = geo_values
  ) |>
    dplyr::transmute(
      as_of = issue,
      location = toupper(geo_value),
      target = paste0("wk inc ", pathogen, " hosp"),
      target_end_date = time_value + 6,
      observation = value
    )
}
