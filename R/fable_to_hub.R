#' Convert fable forecast to hub format
#' Converts a fable object into `model_out_tbl` and `oracle_output` hub format tables.
#' @param cv A time series cross-validation fable object containing the forecasts with `.id` column.
#' @param ts A tsibble containing the observed time series data.
#' @param quantiles Numeric vector. The quantiles to extract from the forecast. Default is c(0.025, 0.25, 0.5, 0.75, 0.975).
#' @return A list with two elements: `model_out_tbl` and `oracle_output`.
#' @importFrom dplyr as_tibble mutate reframe
#' @importFrom tidyr unnest
#' @importFrom stats quantile
#' @keywords internal
#' @noRd

fable_to_hub <- function(
  cv,
  ts,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
) {
  location <- unique(ts$location)
  target <- unique(ts$target)

  model_out_tbl <- cv |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      reference_date = min(target_end_date) - 7,
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
      horizon = as.integer(difftime(
        target_end_date,
        reference_date,
        units = "weeks"
      )),
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
