#' Convert a fable forecast into hub `model_out_tbl` / `oracle_output` tables
#'
#' \code{location} and \code{target} are passed in explicitly so gap-filling
#' never introduces `NA` labels.
#' @param cv A fable of forecasts with an `.id` origin column.
#' @param ts A tsibble of the observed series, for the oracle.
#' @param location,target Single location / target label.
#' @param interval Reporting interval in days; sets the origin offset and
#'   horizon unit.
#' @param quantiles Quantiles to extract. Default c(0.025, 0.25, 0.5, 0.75, 0.975).
#' @return A list with `model_out_tbl` and `oracle_output`.
#' @importFrom dplyr as_tibble mutate reframe filter
#' @importFrom tidyr unnest
#' @importFrom stats quantile
#' @keywords internal
#' @noRd

fable_to_hub <- function(
  cv,
  ts,
  location,
  target,
  interval = 7L,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
) {
  model_out_tbl <- cv |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      reference_date = min(target_end_date) - interval,
      .by = .id
    ) |>
    dplyr::mutate(
      output_type_id = list(as.character(quantiles)),
      value = stats::quantile(observation, quantiles),
    ) |>
    tidyr::unnest(c(output_type_id, value)) |>
    dplyr::reframe(
      model_id = .model,
      reference_date,
      target = target,
      horizon = as.integer(round(
        as.numeric(difftime(target_end_date, reference_date, units = "days")) /
          interval
      )),
      location = location,
      target_end_date,
      output_type = "quantile",
      output_type_id,
      value
    )

  oracle_output <- ts |>
    dplyr::as_tibble() |>
    dplyr::filter(!is.na(observation)) |>
    dplyr::reframe(
      location = location,
      target_end_date,
      target = target,
      output_type = "quantile",
      output_type_id = NA,
      oracle_value = observation
    )

  return(list(model_out_tbl = model_out_tbl, oracle_output = oracle_output))
}
