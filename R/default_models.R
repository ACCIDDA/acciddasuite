#' Default forecasting models
#'
#' The built-in model set for \code{\link{get_cv}} and \code{\link{get_fcast}}:
#' a naive random walk, ETS, Theta and ARIMA, each fitted on
#' \code{log(observation)} to stabilise count variance (\code{fable}
#' back-transforms automatically).
#' Extend rather than replace by composing, e.g.
#' \code{c(default_models(), list(CUSTOM = fable::ARIMA(observation)))}.
#'
#' @return A named list of fable model definitions.
#'
#' @examples
#' default_models()
#'
#' @export
#' @importFrom fable ETS ARIMA NAIVE THETA
default_models <- function() {
  list(
    NAIVE = fable::NAIVE(log(observation)),
    ETS = fable::ETS(log(observation)),
    THETA = fable::THETA(log(observation)),
    ARIMA = fable::ARIMA(log(observation))
  )
}
