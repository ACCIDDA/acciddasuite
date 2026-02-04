#' Evaluate forecast accuracy
#' Internal wrapper around fabletools::accuracy to compute multiple accuracy metrics.
#' @param fcast A fable object containing forecasts.
#' @param ts A tsibble object containing the true values.
#' @param h Integer. Forecast horizon in weeks.
#' @param quantiles Double. A vector of quantiles. Default is c(0.025, 0.25, 0.5, 0.75, 0.975).
#' @return A tibble with accuracy metrics sorted by WIS.
#' @importFrom dplyr arrange
#' @importFrom hubEvals score_model_out
#' @keywords internal
#' @noRd

get_score <- function(
  fcast,
  ts,
  h,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
) {
  fable_to_hub(fcast, ts, h, quantiles) |>
    (\(x) {
      hubEvals::score_model_out(
        model_out_tbl = x$model_out_tbl,
        oracle_output = x$oracle_output,
        metrics = c("wis", "interval_coverage_50", "interval_coverage_95"),
        relative_metrics = "wis",
        by = "model_id"
      )
    })() |>
    dplyr::arrange(wis)
}
