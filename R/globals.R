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
  ".mean",
  ".id",
  "target_end_date",
  "issue",
  "geo_value",
  "time_value",
  "lambda_guerrero",
  "wis",
  "model_id",
  # fable_to_hub / forecasts_key
  "reference_date",
  "horizon",
  "output_type",
  # ground_truth_key
  "oracle_value",
  # forecasts_key / to_respilens / metadata_key
  "target",
  # get_fcast plot
  "q95",
  "q95_lower",
  "q95_upper",
  # get_ncast
  "as_of",
  "confirm",
  "count",
  "delta",
  "ncast_val",
  "ncast_median",
  "ncast_lower",
  "ncast_upper",
  "corrected",
  "pred_count",
  "report_date",
  "location",
  "lower",
  "upper",
  "q25",
  "q75",
  "median",
  # get_fcast scenario pooling
  ".scenario"
))
