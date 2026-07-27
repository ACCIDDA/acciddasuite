#' Produce a forward forecast
#'
#' Fit forecasting models to the full time series and generate forecasts for the
#' next \code{h} reporting intervals.
#'
#' When provided with an \code{accidda_cv} object, the function uses the
#' cross-validation results to select the best-performing models for each series
#' and combines them into an equal-weight ensemble. For \code{accidda_data} or
#' \code{accidda_ncast} objects, all models in \code{models} are fitted and
#' forecast.
#'
#' If the input contains nowcast uncertainty from \code{\link{get_ncast}},
#' this uncertainty is incorporated into the forecast intervals.
#'
#' @param x An \code{accidda_*} object.
#'
#' @param models Named list of \code{fable} model specifications. Defaults to
#'   \code{\link{default_models}}. When \code{x} is an \code{accidda_cv} object,
#'   leave unset to use the top-ranked models from cross-validation, or provide
#'   a custom set of models.
#'
#' @param h Integer giving the forecast horizon in reporting intervals. Defaults
#'   to \code{4}. When \code{x} is an \code{accidda_cv} object, the default is
#'   the cross-validation horizon.
#'
#' @param top_n Integer giving the number of top-ranked models to combine into
#'   the ensemble for each series. Used only when \code{x} is an
#'   \code{accidda_cv} object and \code{models} is not provided. Defaults to
#'   \code{3}.
#'
#' @return An \code{accidda_fcast} object containing:
#' \describe{
#'   \item{hub}{Hub-format forecasts containing \code{model_out_tbl} and
#'   \code{oracle_output}.}
#'   \item{score}{Cross-validation model performance scores, or \code{NULL}.}
#'   \item{meta}{Forecast settings including models, model selection,
#'   ensemble size, horizon, series keys, target, reporting interval,
#'   nowcast information, and evaluation date.}
#' }
#'
#' Forecast outputs can be exported with \code{\link{to_respilens}}.
#'
#' @examples
#' \dontrun{
#' ncast <- get_data("covid", "ny", revisions = TRUE) |> get_ncast()
#' cv <- ncast |> get_cv(eval_start_date = "2025-01-01", h = 4)
#'
#' get_fcast(cv, top_n = 3) # use cross-validation rankings
#' get_fcast(cv, models = default_models()) # use custom models
#' get_fcast(ncast) # forecast directly from nowcast data
#' }
#'
#' @export
#'
#' @importFrom progressr with_progress
#' @importFrom dplyr filter mutate bind_rows summarise coalesce slice_min
#'   semi_join select all_of as_tibble
#' @importFrom fabletools model forecast
#' @importFrom pipetime time_pipe

get_fcast <- function(x, models = default_models(), h = 4, top_n = 3) {
  # The CV supplies each series' top_n models unless the caller passes `models`.
  use_cv_ranking <- inherits(x, "accidda_cv") && missing(models)

  if (inherits(x, "accidda_cv")) {
    if (missing(h)) {
      h <- x$meta$h
    }
    score <- x$score
    meta <- x$meta
    df <- x$data
  } else if (inherits(x, c("accidda_data", "accidda_ncast"))) {
    score <- NULL
    meta <- accidda_meta(x)
    df <- extract_series(x)
  } else {
    stop(
      "`x` must be an accidda_cv, accidda_data or accidda_ncast object.\n",
      "Run check_data() on your data frame first."
    )
  }

  validate_positive_scalar(h, "h", "number of forecast steps")

  key <- meta$key

  # If CV available, select the top_n models per series. Otherwise, validate the provided models.
  if (use_cv_ranking) {
    validate_positive_scalar(top_n, "top_n", "top-ranked models per series")
    selection <- score |>
      dplyr::slice_min(
        wis,
        n = top_n,
        by = dplyr::all_of(key),
        with_ties = FALSE
      ) |>
      dplyr::select(dplyr::all_of(c(key, "model_id")))
    models <- x$models[unique(selection$model_id)]
  } else {
    validate_models(models)
    selection <- NULL
  }

  # Nowcast columns (present when df came from get_ncast)
  has_nowcast <- all(c("ncast_lower", "ncast_upper") %in% names(df))

  # Built once: fitted on below, and the oracle for fable_to_hub.
  ts <- as_model_ts(df, key)

  {
    # --------- Forecast each model on the full series ---------
    progressr::with_progress({
      model_fcast <- if (has_nowcast) {
        pool_nowcast_scenarios(df, key, models, h)
      } else {
        forecast_final(ts, models, h)
      }
    })

    # Keep only each series' selected models before ensembling.
    if (!is.null(selection)) {
      model_fcast <- dplyr::semi_join(
        model_fcast,
        selection,
        by = c(key, ".model" = "model_id")
      )
    }

    # --------- Equal-weight ensemble per series ---------
    ensemble <- model_fcast |>
      dplyr::summarise(
        observation = mix_equally(observation),
        .mean = mean(.mean),
        .by = c(dplyr::all_of(key), target_end_date)
      ) |>
      dplyr::mutate(.model = "ENSEMBLE")

    fcast <- dplyr::bind_rows(model_fcast, ensemble) |>
      dplyr::mutate(.id = 1L)

    new_accidda_fcast(
      hub = fable_to_hub(
        fcast,
        ts,
        key = key,
        target = meta$target,
        interval = meta$interval
      ),
      score = score,
      meta = list(
        models = names(models),
        selection = selection,
        top_n = if (use_cv_ranking) top_n,
        h = h,
        key = key,
        target = meta$target,
        interval = meta$interval,
        nowcast = has_nowcast,
        eval_start_date = meta$eval_start_date
      )
    )
  } |>
    pipetime::time_pipe("get_fcast")
}


#' Fit forecasting models and generate forecasts
#'
#' Fit each model to every series and generate forecasts for the specified
#' horizon.
#'
#' @param ts A keyed model \code{tsibble} created by \code{as_model_ts}.
#' @param models A named list of \code{fable} model specifications.
#' @param h Forecast horizon in reporting intervals.
#'
#' @return A tibble containing forecasts for each series and model, including
#' the model name (\code{.model}), key columns, forecast dates
#' (\code{target_end_date}), observed values, and point forecasts
#' (\code{.mean}).
#'
#' @keywords internal
#' @noRd
forecast_final <- function(ts, models, h) {
  fit <- fabletools::model(ts, !!!models)

  # A model that fails to fit becomes a fable "null model" whose NA forecasts
  # would only crash much later, in the quantile math. Name it and stop here.
  failed <- names(models)[vapply(
    names(models),
    function(m) {
      any(vapply(
        fit[[m]],
        function(x) inherits(x$fit, "null_mdl"),
        logical(1L)
      ))
    },
    logical(1L)
  )]
  if (length(failed) > 0) {
    stop(
      "Model", if (length(failed) > 1) "s " else " ",
      paste(failed, collapse = ", "),
      " failed to fit (fable's warning above says why). ",
      "Fix or drop the model, or use different models."
    )
  }

  fit |>
    fabletools::forecast(h = h) |>
    dplyr::mutate(observation = truncate_counts(observation)) |>
    dplyr::as_tibble()
}


#' Forecast from the three nowcast baselines and pool per (model, series, date)
#' @param df A forecast-ready data frame with `observation`, `ncast_lower`
#'   and `ncast_upper` columns.
#' @param key Key column name(s).
#' @inheritParams forecast_final
#' @return A tibble of pooled per-series, per-model forecasts.
#' @keywords internal
#' @noRd
pool_nowcast_scenarios <- function(df, key, models, h) {
  df_lo <- df |>
    dplyr::mutate(observation = dplyr::coalesce(ncast_lower, observation))
  df_hi <- df |>
    dplyr::mutate(observation = dplyr::coalesce(ncast_upper, observation))

  dplyr::bind_rows(
    forecast_final(as_model_ts(df, key), models, h),
    forecast_final(as_model_ts(df_lo, key), models, h),
    forecast_final(as_model_ts(df_hi, key), models, h)
  ) |>
    dplyr::summarise(
      observation = mix_equally(observation),
      .mean = mean(.mean),
      .by = c(.model, dplyr::all_of(key), target_end_date)
    )
}
