#' Build RespiLens metadata key from `model_out_tbl` data
#'
#' @param model_out_tbl Forecast tibble from `get_fcast()`.
#' @return Named list for RespiLens metadata key
metadata_key <- function(model_out_tbl) {
  # remove peak targets
  df <- model_out_tbl |>
    dplyr::filter(!grepl("peak", target, ignore.case = TRUE))
  loc <- model_out_tbl$location[1]

  list(
    location = loc,
    abbreviation = loc,
    location_name = loc,
    population = 0,
    dataset = "ACCIDDA Suite",
    series_type = "projection",
    hubverse_keys = list(
      models = unique(df$model_id),
      targets = unique(df$target),
      horizons = as.character(unique(df$horizon)),
      output_types = unique(df$output_type)
    )
  )
}


#' Ground truth key
#' @param oracle_output Ground truth tibble from `get_fcast()`.
#' @return JSON-style named list structure to satisfy the RespiLens `ground_truth` key.
#' @noRd
ground_truth_key <- function(oracle_output) {
  if (nrow(oracle_output) == 0) {
    return(list(dates = list()))
  }
  pivot_truth <- oracle_output |>
    dplyr::select(target_end_date, target, oracle_value) |>
    tidyr::pivot_wider(
      names_from = target,
      values_from = oracle_value
    ) |>
    dplyr::arrange(target_end_date)

  ground_truth <- list(
    dates = as.character(pivot_truth$target_end_date)
  )

  target_names <- setdiff(names(pivot_truth), "target_end_date")

  for (target_col in target_names) {
    ground_truth[[target_col]] <- pivot_truth[[target_col]]
  }

  return(ground_truth)
}


#' Forecasts key
#' @param model_out_tbl Forecast tibble from `get_fcast()`.
#' @return JSON-style named list structure to satisfy the RespiLens `forecasts` key.
#' @noRd
forecasts_key <- function(model_out_tbl) {
  # Build JSON structure with nesting
  forecasts <- list()
  groups <- model_out_tbl |>
    dplyr::group_by(reference_date, target, model_id, horizon, output_type) |>
    dplyr::group_split()

  for (df in groups) {
    ref_date <- as.character(df$reference_date[1])
    target_name <- as.character(df$target[1])
    model_name <- as.character(df$model_id[1])
    horizon_val <- as.character(df$horizon[1])
    out_type <- df$output_type[1]

    if (is.null(forecasts[[ref_date]])) {
      forecasts[[ref_date]] <- list()
    }
    if (is.null(forecasts[[ref_date]][[target_name]])) {
      forecasts[[ref_date]][[target_name]] <- list()
    }
    if (is.null(forecasts[[ref_date]][[target_name]][[model_name]])) {
      forecasts[[ref_date]][[target_name]][[model_name]] <- list()
    }
    model_entry <- forecasts[[ref_date]][[target_name]][[model_name]]
    if (out_type == "quantile") {
      model_entry$type <- "quantile"
      if (is.null(model_entry$predictions)) {
        model_entry$predictions <- list()
      }
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(df$target_end_date[1]),
        quantiles = as.numeric(df$output_type_id),
        values = df$value
      )
    } else if (out_type == "pmf") {
      model_entry$type <- "pmf"
      if (is.null(model_entry$predictions)) {
        model_entry$predictions <- list()
      }
      model_entry$predictions[[horizon_val]] <- list(
        date = as.character(df$target_end_date[1]),
        categories = df$output_type_id,
        probabilities = df$value
      )
    } else {
      stop(sprintf(
        "`output_type` must be 'quantile' or 'pmf', received '%s'",
        out_type
      ))
    }
    forecasts[[ref_date]][[target_name]][[model_name]] <- model_entry
  }
  return(forecasts)
}


#' Convert accidda_cast to RespiLens format
#' @param accidda_cast An object of class `accidda_cast`, the output of `get_fcast()`.
#' @param path Optional file path to write the JSON output to. Must end with `.json`.
#' If `NULL`, the output is not written to disk.
#' @return A named list with a single metadata JSON structure and one JSON structure per location.
#' @export

to_respilens <- function(accidda_cast, path = NULL) {
  #check it is of class `accidda_cast`
  if (!inherits(accidda_cast, "accidda_cast")) {
    stop("Input must be an object of class 'accidda_cast'.")
  }

  model_out_tbl = accidda_cast$hubcast$model_out_tbl
  oracle_output = accidda_cast$hubcast$oracle_output

  # Necessary model_out_tbl filtering
  # PATCH: removing 'peaks' keys for now (MyRespiLens doesn't support yet)
  if (any(grepl("peak", model_out_tbl$target, ignore.case = TRUE))) {
    warning(
      "Notice: MyRespiLens does not yet support 'peak' targets. Excluding 'peak' targets from output."
    )
    model_out_tbl <- model_out_tbl |>
      dplyr::filter(!grepl("peak", target, ignore.case = TRUE))
  }
  # also removing output_type==sample (RespiLens doesn't plot this output_type; this will not change)
  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type != "sample")

  model_loc <- unique(model_out_tbl$location)
  gt_loc <- unique(oracle_output$location)

  if (length(model_loc) != 1 || length(gt_loc) != 1) {
    stop("Expected exactly one location in input data.")
  }

  myrespilens <- list(
    metadata = metadata_key(model_out_tbl),
    ground_truth = ground_truth_key(oracle_output),
    forecasts = forecasts_key(model_out_tbl)
  )

  if (!is.null(path)) {
    jsonlite::write_json(myrespilens, path, pretty = TRUE, auto_unbox = TRUE)
  }

  invisible(myrespilens)
}
