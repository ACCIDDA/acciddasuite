#' Nowcast right-truncated surveillance data
#'
#' The most recent weeks of surveillance data are almost always incomplete
#' because of reporting delays (right truncation). \code{get_ncast} uses
#' \href{https://baselinenowcast.epinowcast.org/}{baselinenowcast} to
#' estimate what those counts will look like once all reports arrive.
#'
#' With the default \code{max_delay = 4}, the last 4 weeks are treated as
#' right-truncated and replaced by nowcast estimates. Everything before that
#' is left untouched.
#'
#' The function returns three corrected versions of the full series
#' (nowcast median, lower, and upper 95\% CrI) so that
#' \code{\link{get_fcast}} can propagate nowcast uncertainty into the
#' final forward-looking forecast.
#'
#' @param df An \code{accidda_data} object from \code{\link{check_data}} or
#'   \code{\link{get_data}}. Must have revision history
#'   (\code{$history == TRUE}); use \code{get_data(revisions = TRUE)}.
#' @param max_delay Integer. Maximum reporting delay in weeks. Default 4.
#' @param draws Integer. Number of posterior samples. Default 1000.
#' @param prop_delay Numeric 0-1. Proportion of reference times used for
#'   delay estimation. Default 0.5.
#' @param scale_factor Numeric. Multiplicative factor on the maximum delay
#'   for the estimation window. Default 3.
#'
#' @return An \code{accidda_ncast} object (a list) with:
#'   \describe{
#'     \item{data}{Corrected time series. The \code{observation} column
#'       contains the nowcast median for the corrected weeks.
#'       Two extra columns, \code{ncast_lower} and \code{ncast_upper}
#'       (95\% CrI), are non-NA only for the corrected weeks. These are
#'       used by \code{\link{get_fcast}} to propagate nowcast uncertainty.}
#'     \item{plot}{ggplot2 visualisation of the nowcast correction.}
#'   }
#'
#' @examples
#' \dontrun{
#' df    <- get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
#' ncast <- get_ncast(df)
#' fcast <- get_fcast(ncast, eval_start_date = "2025-01-01")
#' }
#'
#' @export
#' @importFrom dplyr transmute summarise filter select mutate group_by
#'   ungroup left_join coalesce arrange if_else
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs
#'   scale_fill_manual guide_legend theme_classic
#'
get_ncast <- function(
  df,
  max_delay = 4,
  draws = 1000,
  prop_delay = 0.5,
  scale_factor = 3
) {
  if (!requireNamespace("baselinenowcast", quietly = TRUE)) {
    stop(
      "Package 'baselinenowcast' is required.\n",
      "Install with: install.packages('baselinenowcast', ",
      "repos = 'https://epinowcast.r-universe.dev')"
    )
  }

  # Accept accidda_data; coerce plain data frames via check_data()
  if (!inherits(df, "accidda_data")) {
    df <- check_data(df)
  }
  if (!df$history) {
    stop(
      "Nowcasting requires revision history (multiple `as_of` dates).\n",
      "Use get_data(revisions = TRUE) or include an `as_of` column."
    )
  }
  if (max_delay <= 0) {
    stop("`max_delay` must be a positive integer.")
  }

  loc <- df$location
  tgt <- df$target
  df <- df$data

  # Round dates to ISO week so delays are always integer weeks
  week_floor <- function(x) as.Date(cut(x, "week"))

  # --- 1. Use only a recent window for delay estimation ---
  # Older data has fully converged and provides no useful delay information.
  latest_date <- max(df$target_end_date)
  estimation_window <- scale_factor * max_delay * 7 # days
  recent <- dplyr::filter(
    df,
    target_end_date >= latest_date - estimation_window
  )

  # --- 2. Build reporting triangle ---
  # For each (reference_date, report_date) pair, `observation` is the count
  # as known at that report date. Successive revisions typically increase as
  # late reports arrive. Differencing successive revisions for the same
  # reference date yields the incremental new reports per reporting period,
  # which is the input baselinenowcast expects.
  obs <- recent |>
    dplyr::transmute(
      reference_date = week_floor(target_end_date),
      report_date = week_floor(as_of),
      confirm = as.integer(round(observation))
    ) |>
    dplyr::summarise(
      confirm = max(confirm, na.rm = TRUE),
      .by = c(reference_date, report_date)
    ) |>
    dplyr::arrange(reference_date, report_date) |>
    dplyr::group_by(reference_date) |>
    dplyr::mutate(
      delta = confirm - dplyr::lag(confirm, default = 0L),
      count = pmax(0L, delta)
    ) |>
    dplyr::ungroup()

  # Warn if negative revisions were clamped
  n_neg <- sum(obs$delta < 0, na.rm = TRUE)
  if (n_neg > 0) {
    warning(
      "Found ",
      n_neg,
      " negative revision(s) (later report lower than ",
      "earlier one). These were clamped to 0. If this is frequent, the ",
      "data may not follow a monotonically-increasing revision pattern."
    )
  }

  obs <- dplyr::select(obs, reference_date, report_date, count)

  # --- 3. Reporting triangle -> nowcast ---
  rep_tri <- baselinenowcast::as_reporting_triangle(obs, delays_unit = "weeks")
  rep_tri <- baselinenowcast::truncate_to_delay(rep_tri, max_delay = max_delay)

  nowcast_draws <- baselinenowcast::baselinenowcast(
    rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  )

  # --- 4. Summarise draws (including 95% CrI bounds) ---
  ncast_summary <- nowcast_draws |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(
      median = stats::median(pred_count),
      lower = stats::quantile(pred_count, 0.025),
      upper = stats::quantile(pred_count, 0.975),
      q25 = stats::quantile(pred_count, 0.25),
      q75 = stats::quantile(pred_count, 0.75),
      .groups = "drop"
    )

  # --- 5. Build corrected series ---
  # Latest known observation per date (full history)
  best_obs <- df |>
    dplyr::group_by(target_end_date) |>
    dplyr::filter(as_of == max(as_of)) |>
    dplyr::ungroup() |>
    dplyr::select(target_end_date, location, target, observation)

  # Only replace the last max_delay weeks (right-truncated).
  ncast_cutoff <- latest_date - max_delay * 7

  # Join nowcast estimates onto the full series. For corrected weeks
  # (after cutoff): `observation` gets the median, `ncast_lower` /
  # `ncast_upper` get the 95% CrI bounds. For all other weeks these two
  # columns stay NA; get_fcast uses their presence to detect nowcasting.
  ncast_lookup <- data.frame(
    reference_date = ncast_summary$reference_date,
    ncast_median = ncast_summary$median,
    ncast_lower = ncast_summary$lower,
    ncast_upper = ncast_summary$upper
  )

  out_df <- best_obs |>
    dplyr::mutate(reference_date = week_floor(target_end_date)) |>
    dplyr::left_join(ncast_lookup, by = "reference_date") |>
    dplyr::mutate(
      corrected = target_end_date > ncast_cutoff & !is.na(ncast_median),
      observation = dplyr::if_else(corrected, ncast_median, observation),
      ncast_lower = dplyr::if_else(corrected, ncast_lower, NA_real_),
      ncast_upper = dplyr::if_else(corrected, ncast_upper, NA_real_)
    ) |>
    dplyr::select(
      target_end_date,
      location,
      target,
      observation,
      ncast_lower,
      ncast_upper
    ) |>
    dplyr::arrange(target_end_date)

  # --- 6. Plot (estimation window only) ---
  obs_window <- best_obs |>
    dplyr::filter(target_end_date >= latest_date - estimation_window) |>
    dplyr::mutate(reference_date = week_floor(target_end_date))

  p <- ggplot2::ggplot(ncast_summary, ggplot2::aes(x = reference_date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper, fill = "95% CrI"),
      alpha = 0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q25, ymax = q75, fill = "50% CrI"),
      alpha = 0.4
    ) +
    ggplot2::geom_line(ggplot2::aes(y = median)) +
    ggplot2::geom_point(
      data = obs_window,
      ggplot2::aes(x = reference_date, y = observation),
      size = 0.7
    ) +
    ggplot2::scale_fill_manual(
      values = c("95% CrI" = "grey30", "50% CrI" = "grey30"),
      guide = ggplot2::guide_legend(
        override.aes = list(alpha = c(0.2, 0.4))
      )
    ) +
    ggplot2::labs(x = "Date", y = tgt, subtitle = loc, fill = NULL) +
    ggplot2::theme_classic()

  result <- list(data = out_df, plot = p)
  class(result) <- "accidda_ncast"
  result
}
