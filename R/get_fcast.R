#' Rolling origin epidemic forecasts and model evaluation
#'
#' Fits multiple time series models to weekly incidence data and evaluates
#' short term predictive performance using an expanding window rolling
#' origin scheme.
#'
#' From `eval_start_date` onwards, models are repeatedly refitted on all data
#' available up to each evaluation time point and used to forecast the next
#' `h` weeks.
#'
#' Models are ranked by mean WIS score across evaluation periods. The best
#' performing `top_n` models are combined into an equal weight ensemble.
#' A final `h` week ahead forecast is then produced by refitting selected
#' models using the full dataset.
#'
#' @param df Data frame of weekly observations containing
#'   `target_end_date` (Date), `location` (character), `target` (character), and `observation` (numeric).
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
#' @return An object of class `accidda_cast` containing:
#'   \describe{
#'     \item{forecast}{Final `h` week ahead forecasts for all models and the ensemble.}
#'     \item{score}{Model ranking based on rolling origin WIS.}
#'     \item{plot}{ggplot object showing forecasts and prediction intervals.}
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
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon theme_classic
#'
get_fcast <- function(
  df,
  eval_start_date,
  h = 4,
  top_n = 3,
  extra_models = NULL
) {
  # --------- Checks ---------
  eval_start_date <- as.Date(eval_start_date)
  stopifnot(
    is.data.frame(df),
    all(
      c("target_end_date", "observation", "target", "location") %in% names(df)
    ),
    length(unique(df$target)) == 1,
    length(unique(df$location)) == 1,
    is.numeric(h),
    length(h) == 1,
    h > 0,
    is.numeric(top_n),
    length(top_n) == 1,
    top_n > 0,
    inherits(eval_start_date, "Date"),
    is.null(extra_models) || is.list(extra_models)
  )

  ts <- df |>
    dplyr::filter(!is.na(observation)) |>
    tsibble::as_tsibble(index = target_end_date) |>
    tsibble::fill_gaps()

  init <- ts |>
    dplyr::filter(target_end_date < eval_start_date) |>
    nrow()

  if (init < 52) {
    stop("At least 52 weeks of data are required before `eval_start_date`.")
  }

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
  #.init = init = first training window size
  # .step = h = move the cutoff forward by h each time
  # .id = rolling window id
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
  progressr::with_progress(
    model_fcast <- ts |>
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

  ens_fcast <- model_fcast |>
    dplyr::filter(.model %in% top_models) |>
    dplyr::summarise(
      observation = do.call(
        distributional::dist_mixture,
        c(
          as.list(observation),
          list(weights = rep(1 / length(top_models), length(top_models)))
        )
      )
    ) |>
    dplyr::mutate(.model = "ENSEMBLE")

  fcast <- dplyr::bind_rows(model_fcast, ens_fcast) |>
    dplyr::mutate(
      .mean = ifelse(.model == "ENSEMBLE", mean(observation), .mean)
    )

  # --------- Plot ---------
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
    ggplot2::theme_classic()

  # --------- AccidaCast ---------
  acast <- list(
    hubcast = fable_to_hub(
      cv = bind_rows(cv, fcast |> mutate(.id = max(cv$.id) + 1)),
      ts = ts
    ),
    score = score,
    plot = plot
  )
  class(acast) <- "accidda_cast"
  return(acast)
}
