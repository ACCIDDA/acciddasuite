#' Convert fable forecast to hub format
#' Converts a fable object into `model_out_tbl` and `oracle_output` hub format tables.
#' @param fcast A fable object containing the forecast.
#' @param ts A tsibble containing the observed time series data.
#' @param h Integer. The forecast horizon in weeks.
#' @param quantiles Numeric vector. The quantiles to extract from the forecast. Default is c(0.025, 0.25, 0.5, 0.75, 0.975).
#' @return A list with two elements: `model_out_tbl` and `oracle_output`.
#' @importFrom dplyr as_tibble mutate reframe
#' @importFrom tidyr unnest
#' @importFrom stats quantile
#' @keywords internal
#' @noRd

fable_to_hub <- function(
  fcast,
  ts,
  h,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
) {
  location <- unique(ts$location)
  target <- unique(ts$target)

  model_out_tbl <- fcast |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      output_type_id = list(as.character(quantiles)),
      value = stats::quantile(observation, quantiles),
    ) |>
    tidyr::unnest(c(output_type_id, value)) |>
    dplyr::reframe(
      model_id = .model,
      reference_date = target_end_date - (h * 7),
      target = target,
      horizon = h,
      location = location,
      target_end_date,
      output_type = "quantile",
      output_type_id,
      value
    )

  oracle_output <- ts |>
    dplyr::as_tibble() |>
    dplyr::reframe(
      location,
      target_end_date,
      target,
      output_type = "quantile",
      output_type_id = NA,
      oracle_value = observation
    )

  return(list(model_out_tbl = model_out_tbl, oracle_output = oracle_output))
}
