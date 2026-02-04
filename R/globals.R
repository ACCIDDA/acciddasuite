#' Global variables used in NSE functions
#' To avoid R CMD check notes about "no visible binding for global variable"
#' we declare these variables as global here.
#' @keywords internal
#' @noRd

utils::globalVariables(c(
  "observation",
  "output_type_id",
  "value",
  ".model",
  "target_end_date",
  "issue",
  "geo_value",
  "time_value",
  "lambda_guerrero",
  ".id",
  "wis",
  "model_id",
  ".mean",
  "lower",
  "upper",
  "95%"
))
