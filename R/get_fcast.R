#' Produce a forward-looking forecast
#'
#' Refits models on the full series, forecasts \code{h} steps ahead and
#' combines them into an equal-weight ensemble. Accepts either an
#' \code{accidda_cv} from \code{\link{get_cv}} — whose ranking selects the best
#' \code{top_n} models — or an \code{accidda_data} / \code{accidda_ncast},
#' which forecasts every model in \code{models}.
#'
#' If the input carries \code{ncast_lower} / \code{ncast_upper} (from
#' \code{\link{get_ncast}}), forecasts are pooled across the nowcast median and
#' 95\% CrI baselines so the intervals also reflect nowcast uncertainty.
#'
#' @param x An \code{accidda_cv} (\code{\link{get_cv}}), \code{accidda_ncast}
#'   or \code{accidda_data}.
#' @param models Named list of \code{fable} models. Defaults to
#'   \code{\link{default_models}}. Ignored when \code{x} is an
#'   \code{accidda_cv} (its ranked models are reused).
#' @param h Integer. Forecast horizon, in reporting-interval steps (weeks for
#'   weekly data). Default 4; for an \code{accidda_cv}, defaults to the
#'   cross-validation horizon.
#' @param top_n Integer. Number of top-ranked models to ensemble; used only
#'   when \code{x} is an \code{accidda_cv}. Default 3.
#'
#' @return An \code{accidda_fcast} object:
#'   \describe{
#'     \item{hub}{Hub-format forecast (\code{model_out_tbl}, \code{oracle_output}).}
#'     \item{score}{Model ranking from the \code{accidda_cv}, or \code{NULL}.}
#'     \item{meta}{\code{models}, \code{top_n}, \code{h}, \code{location},
#'       \code{target}, \code{interval}, \code{nowcast}.}
#'   }
#'   Export with \code{\link{to_respilens}}.
#'
#' @examples
#' \dontrun{
#' ncast <- get_data("covid", "ny", revisions = TRUE) |> get_ncast()
#' cv    <- ncast |> get_cv(eval_start_date = "2025-01-01", h = 4)
#'
#' get_fcast(cv, top_n = 3)   # reuse the cross-validation ranking
#' get_fcast(ncast)           # or forecast the default models directly
#' }
#'
#' @export
#'
#' @importFrom progressr with_progress
#' @importFrom dplyr filter mutate pull arrange slice_head bind_rows summarise
#'   coalesce select as_tibble
#' @importFrom tsibble as_tsibble fill_gaps
#' @importFrom fabletools model forecast
#' @importFrom pipetime time_pipe
#'
get_fcast <- function(x, models = default_models(), h = 4, top_n = 3) {
  if (inherits(x, "accidda_cv")) {
    if (missing(h)) {
      h <- x$meta$h
    }
    if (!is.numeric(top_n) || length(top_n) != 1L || top_n <= 0) {
      stop("`top_n` must be a single positive integer.")
    }
    top_ids <- x$score |>
      dplyr::arrange(wis) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::pull(model_id)
    models <- x$models[top_ids]
    df <- x$data
    score <- x$score
    meta <- x$meta
  } else {
    if (!inherits(x, c("accidda_data", "accidda_ncast"))) {
      stop(
        "`x` must be an accidda_cv, accidda_data or accidda_ncast object.\n",
        "Run check_data() on your data frame first."
      )
    }
    validate_models(models)
    df <- extract_series(x)
    score <- NULL
    meta <- accidda_meta(x)
  }

  if (!is.numeric(h) || length(h) != 1L || h <= 0) {
    stop("`h` must be a single positive number (number of forecast steps).")
  }

  # Nowcast columns (present when df came from get_ncast)
  has_nowcast <- all(c("ncast_lower", "ncast_upper") %in% names(df))

  # Time the forecast with pipetime.
  {
    # --------- Forecast each model on the full series ---------
    model_fcast <- if (has_nowcast) {
      pool_nowcast_scenarios(df, models, h)
    } else {
      forecast_final(df, models, h)
    }

    # --------- Equal-weight ensemble of the chosen models ---------
    ensemble <- model_fcast |>
      dplyr::filter(.model %in% names(models)) |>
      dplyr::summarise(
        observation = mix_equally(observation),
        .mean = mean(.mean),
        .by = target_end_date
      ) |>
      dplyr::mutate(.model = "ENSEMBLE")

    fcast <- dplyr::bind_rows(model_fcast, ensemble) |>
      dplyr::mutate(.id = 1L)

    # Observed series, for the hub oracle.
    ts <- as_model_ts(df)

    new_accidda_fcast(
      hub = fable_to_hub(
        fcast,
        ts,
        location = meta$location,
        target = meta$target,
        interval = meta$interval
      ),
      score = score,
      meta = list(
        models = names(models),
        top_n = if (inherits(x, "accidda_cv")) top_n else length(models),
        h = h,
        location = meta$location,
        target = meta$target,
        interval = meta$interval,
        nowcast = has_nowcast,
        eval_start_date = if (inherits(x, "accidda_cv")) {
          x$meta$eval_start_date
        } else {
          NULL
        }
      )
    )
  } |>
    pipetime::time_pipe("get_fcast")
}


#' Refit models on the full series and forecast `h` steps ahead
#' @param df A forecast-ready data frame with an `observation` column.
#' @param models A named list of fable model definitions.
#' @param h Forecast horizon, in reporting-interval steps.
#' @return A tibble of per-model forecasts (`.model`, `target_end_date`,
#'   `observation`, `.mean`).
#' @keywords internal
#' @noRd
forecast_final <- function(df, models, h) {
  ts <- as_model_ts(df)

  progressr::with_progress(
    fcast <- ts |>
      fabletools::model(!!!models) |>
      fabletools::forecast(h = h) |>
      dplyr::mutate(observation = truncate_counts(observation))
  )
  dplyr::as_tibble(fcast)
}


#' Forecast from the three nowcast baselines and pool per (model, date)
#' @inheritParams forecast_final
#' @return A tibble of pooled per-model forecasts.
#' @keywords internal
#' @noRd
pool_nowcast_scenarios <- function(df, models, h) {
  df_lo <- df |>
    dplyr::mutate(observation = dplyr::coalesce(ncast_lower, observation))
  df_hi <- df |>
    dplyr::mutate(observation = dplyr::coalesce(ncast_upper, observation))

  dplyr::bind_rows(
    forecast_final(df, models, h),
    forecast_final(df_lo, models, h),
    forecast_final(df_hi, models, h)
  ) |>
    dplyr::summarise(
      observation = mix_equally(observation),
      .mean = mean(.mean),
      .by = c(.model, target_end_date)
    )
}
