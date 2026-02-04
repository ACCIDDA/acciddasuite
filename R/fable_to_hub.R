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
