#' Default forecasting models
#'
#' Return the default set of forecasting models used by
#' \code{\link{get_cv}} and \code{\link{get_fcast}}.
#'
#' The default models are naive, ETS, Theta, and ARIMA models. All models are
#' fitted to \code{log(observation + 1)} to stabilise variance in count data.
#' Forecasts are automatically transformed back to the original scale by
#' \code{fable}. +1 is added to avoid taking the log of zero.
#'
#' Additional models can be added by extending the returned list, for example:
#' \code{c(default_models(), list(CUSTOM = fable::ARIMA(observation)))}.
#'
#' @author Cyril Geismar
#' 
#' @return A named list of \code{fable} model specifications.
#'
#' @examples
#' default_models()
#'
#' @export
#' @importFrom fable ETS ARIMA NAIVE THETA
#' @importFrom feasts unitroot_ndiffs
default_models <- function() {
  list(
    NAIVE = fable::NAIVE(log(observation + 1)),
    ETS = fable::ETS(log(observation + 1)),
    THETA = fable::THETA(log(observation + 1)),
    ARIMA = fable::ARIMA(log(observation + 1))
  )
}
