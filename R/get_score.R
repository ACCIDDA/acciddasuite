#' Evaluate forecast accuracy
#' Internal wrapper around fabletools::accuracy to compute multiple accuracy metrics.
#' @param fcast A fable object containing forecasts.
#' @param ts A tsibble object containing the true values.
#' @return A tibble with accuracy metrics sorted by CRPS.
#' @importFrom fabletools accuracy CRPS winkler_score RMSE MAE
#' @importFrom dplyr arrange
#' @keywords internal
#' @noRd

get_score <- function(fcast, ts) {
  fcast |>
    fabletools::accuracy(
      data = ts,
      measures = list(
        CRPS = fabletools::CRPS,
        WINKLER = fabletools::winkler_score,
        RMSE = fabletools::RMSE,
        MAE = fabletools::MAE
      )
    ) |>
    dplyr::arrange(CRPS)
}
