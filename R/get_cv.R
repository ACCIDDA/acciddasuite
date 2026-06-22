#' Cross-validate forecasting models
#'
#' Evaluates \code{fable} models by expanding-window time-series
#' cross-validation: from \code{eval_start_date}, models are refit at each
#' origin and forecast \code{h} steps ahead, then scored by weighted interval
#' score (WIS) and interval coverage via \code{hubEvals}. The ranking is
#' reused by \code{\link{get_fcast}}.
#'
#' @param x An \code{accidda_ncast} (\code{\link{get_ncast}}) or
#'   \code{accidda_data} (\code{\link{check_data}} / \code{\link{get_data}}).
#' @param eval_start_date Date (or string coercible to one). First origin
#'   evaluated; must lie within the data window. All observations before it form
#'   the initial training window.
#' @param h Integer. Forecast horizon, in reporting-interval steps (weeks for
#'   weekly data, days for daily). Default 4.
#' @param models Named list of \code{fable} models. Defaults to
#'   \code{\link{default_models}}; compose with \code{c(default_models(),
#'   list(...))} to extend. Each element must reference \code{observation}. See
#'   the \href{https://fabletools.tidyverts.org/articles/extension_models.html}{fabletools extension vignette} for custom models.
#' @param step Integer. Number of reporting-interval steps the training window
#'   advances between successive CV origins. Defaults to \code{h}
#'   (non-overlapping evaluation blocks).
#'
#' @return An \code{accidda_cv} object:
#'   \describe{
#'     \item{forecasts}{Per-origin, per-model forecasts (\code{model_out_tbl}).}
#'     \item{oracle}{Observed truth (\code{oracle_output}).}
#'     \item{score}{Model ranking by WIS with interval coverage.}
#'     \item{models}{The evaluated model specifications.}
#'     \item{meta}{\code{eval_start_date}, \code{h}, \code{location},
#'       \code{target}, \code{interval}.}
#'     \item{data}{Revision-collapsed input, reused by \code{\link{get_fcast}}.}
#'   }
#'
#' @examples
#' \dontrun{
#' cv <- get_data("covid", "ny", revisions = TRUE) |>
#'   get_ncast() |>
#'   get_cv(eval_start_date = "2025-01-01", h = 4)
#'
#' cv$score
#' }
#'
#' @export
#'
#' @importFrom progressr with_progress progressor
#' @importFrom dplyr filter group_by ungroup select arrange
#' @importFrom tsibble as_tsibble fill_gaps stretch_tsibble
#' @importFrom fabletools model forecast
#' @importFrom hubEvals score_model_out
#' @importFrom pipetime time_pipe
#'
get_cv <- function(
  x,
  eval_start_date,
  h = 4,
  models = default_models(),
  step = h
) {
  if (!inherits(x, c("accidda_data", "accidda_ncast"))) {
    stop(
      "`x` must be an accidda_data or accidda_ncast object.\n",
      "Run check_data() on your data frame first."
    )
  }

  eval_start_date <- as.Date(eval_start_date)
  if (length(eval_start_date) != 1L || is.na(eval_start_date)) {
    stop("`eval_start_date` must be a single date.")
  }
  if (!is.numeric(h) || length(h) != 1L || h <= 0) {
    stop("`h` must be a single positive number (number of forecast steps).")
  }
  if (!is.numeric(step) || length(step) != 1L || step <= 0) {
    stop(
      "`step` must be a single positive number (periods between CV origins)."
    )
  }
  validate_models(models)

  meta <- accidda_meta(x)
  df <- extract_series(x)

  # eval_start_date must sit inside the observed window.
  data_from <- min(df$target_end_date)
  data_to <- max(df$target_end_date)
  if (eval_start_date <= data_from || eval_start_date > data_to) {
    stop(
      "`eval_start_date` (",
      as.character(eval_start_date),
      ") must fall within the data window (",
      as.character(data_from),
      " to ",
      as.character(data_to),
      ")."
    )
  }

  # Time the cross-validation (fit + score) with pipetime.
  {
    # Build the regular tsibble from the (median-corrected) observation series.
    ts <- as_model_ts(df)

    # One progress session spans the two slow phases. Model fitting emits
    # fable's own per-origin progressr bar; scoring is not progressr-instrumented
    # (hubEvals/scoringutils don't use it), so we announce it with an explicit
    # step created after fitting, so it never competes with fable's bar.
    progressr::with_progress({
      fcast <- ts |>
        make_cv_origins(eval_start_date, step = step) |>
        fabletools::model(!!!models) |>
        fabletools::forecast(h = h) |>
        dplyr::mutate(observation = truncate_counts(observation))

      # Build the hub once; reused for both the stored forecasts and the score.
      hub <- fable_to_hub(
        fcast,
        ts,
        location = meta$location,
        target = meta$target,
        interval = meta$interval
      )

      p <- progressr::progressor(steps = 1)
      p(message = "Scoring forecasts")
      score <- hubEvals::score_model_out(
        model_out_tbl = hub$model_out_tbl,
        oracle_output = hub$oracle_output,
        metrics = c("wis", "interval_coverage_50", "interval_coverage_95"),
        relative_metrics = "wis",
        by = "model_id"
      ) |>
        dplyr::arrange(wis)
    })

    new_accidda_cv(
      forecasts = hub$model_out_tbl,
      oracle = hub$oracle_output,
      score = score,
      models = models,
      meta = list(
        eval_start_date = eval_start_date,
        h = h,
        location = meta$location,
        target = meta$target,
        interval = meta$interval
      ),
      data = df
    )
  } |>
    pipetime::time_pipe("get_cv")
}


#' Forecast-ready data frame from a typed object
#'
#' Returns the data frame, keeping the latest revision per
#' \code{target_end_date} when revision history is present.
#' @param x An \code{accidda_data} or \code{accidda_ncast} object.
#' @return A data frame, one row per \code{target_end_date}.
#' @keywords internal
#' @noRd
extract_series <- function(x) {
  if (inherits(x, "accidda_ncast") || inherits(x, "accidda_data")) {
    df <- x$data
  } else {
    stop(
      "`x` must be an accidda_data or accidda_ncast object.\n",
      "Run check_data() on your data frame first."
    )
  }

  if ("as_of" %in% names(df)) {
    df <- df |>
      dplyr::group_by(target_end_date) |>
      dplyr::filter(as_of == max(as_of)) |>
      dplyr::ungroup() |>
      dplyr::select(-as_of)
  }
  df
}


#' Build expanding-window cross-validation origins
#'
#' All observations before \code{eval_start_date} form the initial training
#' window; each subsequent origin extends it by \code{step}. The final
#' incomplete origin is dropped, so \code{eval_start_date} must leave at least
#' one full evaluation window.
#' @param ts A tsibble of the observation series.
#' @param eval_start_date First evaluated origin.
#' @param step Number of steps between origins.
#' @return A stretched tsibble keyed by `.id`.
#' @keywords internal
#' @noRd
make_cv_origins <- function(ts, eval_start_date, step) {
  init <- ts |>
    dplyr::filter(target_end_date < eval_start_date) |>
    nrow()

  # eval_start_date alone sets the initial training window (all data before it);
  # only guard the other end, so at least one full fold remains to be scored.
  if (init > nrow(ts) - step) {
    stop(sprintf(
      "`eval_start_date` leaves too little to score: %d periods after it (need >= %d).",
      nrow(ts) - init,
      step
    ))
  }

  ts |>
    tsibble::stretch_tsibble(.init = init, .step = step) |>
    dplyr::filter(.id != max(.id))
}
