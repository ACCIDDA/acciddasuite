#' Forecast and evaluate time series models
#'
#' Selects and evaluates multiple time series models via expanding-window
#' cross-validation, then produces a final forward-looking forecast.
#'
#' **Cross-validation.** From \code{eval_start_date} onwards, models are
#' repeatedly refitted on all data up to each cutoff and used to forecast
#' the next \code{h} weeks. Models are ranked by WIS; the best \code{top_n}
#' form an equal-weight ensemble.
#'
#' **Nowcast uncertainty.** When an \code{accidda_ncast} object is
#' supplied (or an \code{accidda_data} whose data frame contains
#' \code{ncast_lower} and \code{ncast_upper} columns),
#' cross-validation runs on the \code{observation} column (the median
#' corrected series). The final forecast is then produced three times
#' (from the median, lower 95\% CrI, and upper 95\% CrI baselines)
#' and the resulting distributions are pooled, so prediction intervals
#' reflect both model uncertainty and nowcast uncertainty.
#'
#' @param df An \code{accidda_ncast} object from
#'   \code{\link{get_ncast}}, or an \code{accidda_data} object from
#'   \code{\link{check_data}} or \code{\link{get_data}}.
#'   If the underlying data frame contains \code{ncast_lower} and
#'   \code{ncast_upper} columns, nowcast uncertainty propagation
#'   is enabled automatically.
#' @param eval_start_date Date or string coercible to Date. First date at
#'   which forecasts are evaluated. At least 52 weeks of data must precede
#'   this date.
#' @param h Integer. Forecast horizon in weeks. Default is 4.
#' @param top_n Integer. Number of top ranked models included in the
#'   ensemble. Default is 3.
#' @param extra_models Named list of additional forecasting models passed to
#'   `fabletools::model()`. Each element must reference the `observation`
#'   column and be compatible with `fabletools::forecast()`.
#'
#' Custom models can be constructed using the fable modelling framework,
#' see the \href{https://fabletools.tidyverts.org/articles/extension_models.html}{fabletools extension_models vignette}.
#'
#'   The name of each list element is used as the model label in the output.
#'
#' @return An object of class `accidda_fcast` containing:
#'   \describe{
#'     \item{hubcast}{Hub-format forecast object (`model_out_tbl` and `oracle_output`).}
#'     \item{score}{Model ranking based on rolling origin WIS.}
#'     \item{plot}{ggplot2 object showing forecasts and prediction intervals.}
#'   }
#'
#' @examples
#' \dontrun{
#' extra_models <- list(
#'   CUSTOM_ARIMA = ARIMA(observation ~ pdq(1, 1, 0)),
#'   PROPHET = fable.prophet::PROPHET(observation),
#'   EPIESTIM = EPIESTIM(observation, mean_si = 3.96, std_si = 4.75)
#' )
#'
#' get_fcast(
#'   df,
#'   eval_start_date = "2025-01-01",
#'   h = 4,
#'   top_n = 3,
#'   extra_models = extra_models
#' )
#' }
#'
#' @export
#'
#' @importFrom rlang inject !!
#' @importFrom progressr with_progress
#' @importFrom dplyr filter mutate pull arrange slice_head bind_rows
#' @importFrom tsibble as_tsibble fill_gaps stretch_tsibble
#' @importFrom fable ETS ARIMA SNAIVE THETA
#' @importFrom fabletools features box_cox model forecast hilo
#' @importFrom feasts guerrero
#' @importFrom distributional dist_mixture
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon labs
#'   theme_classic
#'
get_fcast <- function(
  df,
  eval_start_date,
  h = 4,
  top_n = 3,
  extra_models = NULL
) {
  # --------- Extract df from typed objects ---------
  if (inherits(df, "accidda_ncast")) {
    df <- df$data
  } else if (inherits(df, "accidda_data")) {
    df <- df$data
  } else {
    stop(
      "`df` must be an accidda_data or accidda_ncast object.\n",
      "Run check_data() on your data frame first."
    )
  }

  # If revision history is present (multiple as_of per date), keep only
  # the latest revision for each target_end_date.
  if ("as_of" %in% names(df)) {
    df <- df |>
      dplyr::group_by(target_end_date) |>
      dplyr::filter(as_of == max(as_of)) |>
      dplyr::ungroup() |>
      dplyr::select(-as_of)
  }

  # Detect nowcast columns (present when df came from get_ncast)
  has_nowcast <- all(c("ncast_lower", "ncast_upper") %in% names(df))

  # --------- Checks ---------
  eval_start_date <- as.Date(eval_start_date)
  stopifnot(
    is.numeric(h),
    length(h) == 1,
    h > 0,
    is.numeric(top_n),
    length(top_n) == 1,
    top_n > 0,
    inherits(eval_start_date, "Date"),
    is.null(extra_models) || is.list(extra_models)
  )

  # --------- Build tsibble from median-corrected series ---------
  ts <- df |>
    dplyr::filter(!is.na(observation)) |>
    tsibble::as_tsibble(index = target_end_date) |>
    tsibble::fill_gaps()

  # --------- Transformation ---------
  lambda_val <- ts |>
    fabletools::features(observation, feasts::guerrero) |>
    dplyr::pull(lambda_guerrero) |>
    (\(x) ifelse(is.na(x) | x == 1, 0, x))()

  # --------- Prepare models ---------
  default_models <- list(
    SNAIVE = fable::SNAIVE(
      fabletools::box_cox(observation, !!lambda_val) ~ lag(52)
    ),
    ETS = fable::ETS(fabletools::box_cox(observation, !!lambda_val)),
    THETA = fable::THETA(fabletools::box_cox(observation, !!lambda_val)),
    ARIMA = fable::ARIMA(fabletools::box_cox(observation, !!lambda_val))
  )

  all_models <- c(default_models, extra_models)

  # --------- Time Series Cross Validation ---------
  # CV runs on the full median-corrected series (ts). When nowcasting was
  # applied, the recent right-truncated weeks use the nowcast median (the
  # best available estimate) so models train and are evaluated on the most
  # complete picture of the epidemic curve.
  # Nowcast uncertainty is NOT propagated here; it only affects the final
  # forward-looking forecast (see scenario pooling below).
  init <- ts |>
    dplyr::filter(target_end_date < eval_start_date) |>
    nrow()

  if (init < 52) {
    stop("At least 52 weeks of data are required before `eval_start_date`.")
  }

  # .init = first training window size
  # .step = h = move the cutoff forward by h each time
  progressr::with_progress(
    model_cv <- ts |>
      tsibble::stretch_tsibble(.init = init, .step = h) |>
      dplyr::filter(.id != max(.id)) |>
      fabletools::model(!!!all_models) |>
      fabletools::forecast(h = h) |>
      dplyr::mutate(
        observation = distributional::dist_truncated(
          observation,
          lower = 0,
          upper = Inf
        )
      )
  )

  top_models <- get_score(model_cv, ts) |>
    dplyr::arrange(wis) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(model_id)

  ens_cv <- model_cv |>
    dplyr::filter(.model %in% top_models) |>
    as_tibble() |>
    dplyr::summarise(
      observation = do.call(
        distributional::dist_mixture,
        c(
          as.list(observation),
          list(weights = rep(1 / length(top_models), length(top_models)))
        )
      ),
      .by = c(.id, target_end_date)
    ) |>
    dplyr::mutate(.model = "ENSEMBLE")

  cv <- dplyr::bind_rows(model_cv, ens_cv)

  score <- get_score(cv, ts) |>
    dplyr::arrange(wis)

  # --------- Final forecast ---------
  # Helper: fit all models on a series and forecast h steps ahead
  forecast_from <- function(input_df) {
    ts_s <- input_df |>
      dplyr::filter(!is.na(observation)) |>
      dplyr::select(target_end_date, observation) |>
      tsibble::as_tsibble(index = target_end_date) |>
      tsibble::fill_gaps()
    progressr::with_progress(
      fcast <- ts_s |>
        fabletools::model(!!!all_models) |>
        fabletools::forecast(h = h) |>
        dplyr::mutate(
          observation = distributional::dist_truncated(
            observation,
            lower = 0,
            upper = Inf
          )
        )
    )
    dplyr::as_tibble(fcast)
  }

  if (has_nowcast) {
    # --- Nowcast path: forecast from 3 baselines, pool ---
    # Build lower/upper variants by swapping observation where corrected
    df_lo <- df |>
      dplyr::mutate(observation = dplyr::coalesce(ncast_lower, observation))
    df_hi <- df |>
      dplyr::mutate(observation = dplyr::coalesce(ncast_upper, observation))

    fcast_med <- forecast_from(df)
    fcast_lo <- forecast_from(df_lo)
    fcast_hi <- forecast_from(df_hi)

    # Pool: for each (model, date), mix the three scenario distributions
    # to widen intervals by nowcast uncertainty.
    model_fcast <- dplyr::bind_rows(
      fcast_med |> dplyr::mutate(.scenario = "median"),
      fcast_lo |> dplyr::mutate(.scenario = "lower"),
      fcast_hi |> dplyr::mutate(.scenario = "upper")
    ) |>
      dplyr::summarise(
        observation = do.call(
          distributional::dist_mixture,
          c(as.list(observation), list(weights = rep(1 / 3, 3)))
        ),
        .mean = mean(.mean),
        .by = c(.model, target_end_date)
      )
  } else {
    # --- Standard path: single forecast ---
    model_fcast <- forecast_from(df)
  }

  # --------- Ensemble (from pooled or single forecasts) ---------
  ens_fcast <- model_fcast |>
    dplyr::filter(.model %in% top_models) |>
    dplyr::summarise(
      observation = do.call(
        distributional::dist_mixture,
        c(
          as.list(observation),
          list(weights = rep(1 / length(top_models), length(top_models)))
        )
      ),
      .mean = mean(.mean),
      .by = target_end_date
    ) |>
    dplyr::mutate(.model = "ENSEMBLE")

  fcast <- dplyr::bind_rows(model_fcast, ens_fcast) |>
    dplyr::mutate(
      .mean = ifelse(
        .model == "ENSEMBLE",
        mean(observation),
        .mean
      )
    )

  # --------- Plot ---------
  # Nowcast ribbon data (grey band showing nowcast uncertainty on recent obs)
  ncast_ribbon <- if (has_nowcast) {
    df |>
      dplyr::filter(!is.na(ncast_lower)) |>
      dplyr::select(target_end_date, observation, ncast_lower, ncast_upper)
  } else {
    NULL
  }

  plot <- fcast |>
    dplyr::filter(.model %in% c(top_models, "ENSEMBLE")) |>
    as_tibble() |>
    mutate(q95 = fabletools::hilo(observation, level = 95)) |>
    fabletools::unpack_hilo(q95) |>
    mutate(
      .model = factor(
        .model,
        levels = score |>
          dplyr::filter(model_id %in% c(top_models, "ENSEMBLE")) |>
          dplyr::arrange(wis) |>
          dplyr::pull(model_id)
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = target_end_date)) +
    # Nowcast uncertainty ribbon (behind everything else)
    {
      if (!is.null(ncast_ribbon)) {
        ggplot2::geom_ribbon(
          data = ncast_ribbon,
          ggplot2::aes(ymin = ncast_lower, ymax = ncast_upper),
          fill = "grey70",
          alpha = 0.4
        )
      }
    } +
    ggplot2::geom_line(
      data = ts |> dplyr::filter(target_end_date >= eval_start_date),
      ggplot2::aes(y = observation),
      colour = "black"
    ) +
    ggplot2::geom_point(
      data = ts |> dplyr::filter(target_end_date >= eval_start_date),
      ggplot2::aes(y = observation),
      colour = "black",
      size = 0.7
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q95_lower, ymax = q95_upper, fill = .model),
      alpha = 0.2
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .mean, colour = .model)) +
    ggplot2::geom_point(ggplot2::aes(y = .mean, colour = .model), size = 0.7) +
    ggplot2::labs(fill = "95% CrI", colour = "95% CrI") +
    ggplot2::theme_classic()

  # --------- accida_fcast ---------
  a_fcast <- list(
    hubcast = fable_to_hub(
      cv = bind_rows(cv, fcast |> mutate(.id = max(cv$.id) + 1)),
      ts = ts
    ),
    score = score,
    plot = plot
  )
  class(a_fcast) <- "accidda_fcast"
  return(a_fcast)
}
