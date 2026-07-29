#' Nowcast right-truncated surveillance data
#'
#' Recent weeks of surveillance data are incomplete because of reporting
#' delays (right truncation). \code{get_ncast} estimates their final counts
#' with \href{https://baselinenowcast.epinowcast.org/}{baselinenowcast},
#' replacing the last \code{max_delay} weeks of every series and leaving
#' earlier weeks untouched. Downward revisions are redistributed across
#' earlier delays via
#' \code{\link[baselinenowcast]{preprocess_negative_values}}.
#'
#' Weekly data only; other cadences are rejected (the rest of the pipeline is
#' cadence-agnostic).
#'
#' @param x An \code{incast_data} (\code{\link{get_data}} /
#'   \code{\link{check_data}}) with revision history; use
#'   \code{get_data(revisions = TRUE)}.
#' @param max_delay Integer. Number of recent weeks treated as
#'   right-truncated. Default 2.
#' @param draws Integer. Number of posterior samples. Default 1000.
#' @param prop_delay Numeric in (0, 1). Proportion of reference times used for
#'   delay estimation. Default 0.5.
#' @param scale_factor Numeric. Multiplier on \code{max_delay} for the
#'   estimation window. Default 3.
#'
#' @return An \code{incast_ncast} object with the shared backbone
#'   (\code{key}, \code{target}, \code{window}, \code{interval},
#'   \code{history}) plus:
#'   \describe{
#'     \item{data}{Corrected series. \code{observation} holds the nowcast
#'       median for corrected weeks; \code{ncast_lower} / \code{ncast_upper}
#'       (95\% CrI) are non-NA only there, and let \code{\link{get_fcast}}
#'       propagate nowcast uncertainty.}
#'     \item{meta}{The nowcast settings (\code{max_delay}, \code{draws},
#'       \code{prop_delay}, \code{scale_factor}) and \code{ncast_summary},
#'       one tidy table of per-series weekly nowcasts (median, CrI bounds and
#'       the reported-so-far \code{observed} count) plotted by
#'       \code{\link[ggplot2]{autoplot}}.}
#'   }
#'
#' @examples
#' \dontrun{
#' x <- get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
#' ncast <- get_ncast(x)
#' autoplot(ncast)
#' }
#'
#' @export
get_ncast <- function(
  x,
  max_delay = 2,
  draws = 1000,
  prop_delay = 0.5,
  scale_factor = 3
) {
  # Accept incast_data; coerce plain data frames via check_data()
  x <- check_data(x)

  if (!x$history) {
    stop(
      "Nowcasting requires revision history (multiple `as_of` dates).\n",
      "Use get_data(revisions = TRUE) or include an `as_of` column."
    )
  }
  if (max_delay <= 0) {
    stop("`max_delay` must be a positive integer.")
  }
  # Nowcasting is weekly-only.
  if (x$interval != 7L) {
    stop(
      "get_ncast() currently supports weekly data only ",
      "(detected reporting interval = ",
      x$interval,
      " days).\n",
      "Aggregate the series to weekly before nowcasting, or skip the nowcast ",
      "and pass the data straight to get_cv() / get_fcast()."
    )
  }

  # Latest known value per week of every series, with its reference week.
  best_obs <- extract_series(x) |>
    dplyr::mutate(reference_date = week_floor(target_end_date))

  # Weekly nowcast per series -- the only per-series step: baselinenowcast
  # models one reporting triangle at a time.
  ncast_summary <- x$data |>
    dplyr::reframe(
      run_ncast(
        dplyr::pick(dplyr::everything()),
        max_delay, draws, prop_delay, scale_factor
      ),
      .by = dplyr::all_of(x$key)
    ) |>
    dplyr::left_join(
      dplyr::select(
        best_obs,
        dplyr::all_of(x$key),
        reference_date,
        observed = observation
      ),
      by = c(x$key, "reference_date")
    )

  # Splice: the last max_delay weeks of each series take the nowcast median
  # and 95% CrI bounds; earlier weeks keep their observation and NA bounds.
  data <- best_obs |>
    dplyr::left_join(
      dplyr::select(
        ncast_summary,
        dplyr::all_of(x$key),
        reference_date,
        ncast_median = median,
        ncast_lower = lower,
        ncast_upper = upper
      ),
      by = c(x$key, "reference_date")
    ) |>
    dplyr::mutate(
      corrected = !is.na(ncast_median) &
        target_end_date > max(target_end_date) - max_delay * x$interval,
      .by = dplyr::all_of(x$key)
    ) |>
    dplyr::mutate(
      observation = dplyr::if_else(corrected, ncast_median, observation),
      ncast_lower = dplyr::if_else(corrected, ncast_lower, NA_real_),
      ncast_upper = dplyr::if_else(corrected, ncast_upper, NA_real_)
    ) |>
    dplyr::select(-reference_date, -ncast_median, -corrected) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(x$key, "target_end_date"))))

  new_incast_ncast(
    data = data,
    key = x$key,
    target = x$target,
    # Splicing replaces values, never dates, so the window is unchanged.
    window = x$window,
    interval = x$interval,
    history = TRUE,
    meta = list(
      max_delay = max_delay,
      draws = draws,
      prop_delay = prop_delay,
      scale_factor = scale_factor,
      ncast_summary = ncast_summary
    )
  )
}


#' Round dates to ISO week so reporting delays are always integer weeks
#' @keywords internal
#' @noRd
week_floor <- function(dates) {
  as.Date(cut(dates, "week"))
}


#' Nowcast one series: reporting triangle -> posterior draws -> weekly summary
#'
#' @param df Revision history (`target_end_date`, `as_of`, `observation`) for
#'   a single series.
#' @inheritParams get_ncast
#' @return A tibble with one row per reference week: `median`, `lower` /
#'   `upper` (95\% CrI) and `q25` / `q75`.
#' @keywords internal
#' @noRd
run_ncast <- function(df, max_delay, draws, prop_delay, scale_factor) {
  # Only a recent window is used for delay estimation.
  estimation_window <- scale_factor * max_delay * 7L # days

  rep_tri <- df |>
    build_reporting_triangle(estimation_window) |>
    baselinenowcast::as_reporting_triangle(delays_unit = "weeks") |>
    baselinenowcast::preprocess_negative_values() |>
    baselinenowcast::truncate_to_delay(max_delay = max_delay)

  baselinenowcast::baselinenowcast(
    rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  ) |>
    dplyr::summarise(
      median = stats::median(pred_count),
      lower = stats::quantile(pred_count, 0.025),
      upper = stats::quantile(pred_count, 0.975),
      q25 = stats::quantile(pred_count, 0.25),
      q75 = stats::quantile(pred_count, 0.75),
      .by = reference_date
    )
}


#' Build the incremental reporting triangle for baselinenowcast
#'
#' Differences successive revisions per reference date into new reports per
#' week. Negative increments (downward revisions) are kept;
#' \code{baselinenowcast::preprocess_negative_values()} redistributes them.
#' @param df Revision history (`as_of`) for a single series.
#' @param estimation_window Width in days of the delay-estimation window.
#' @return A data frame of `reference_date`, `report_date`, `count`.
#' @keywords internal
#' @noRd
build_reporting_triangle <- function(df, estimation_window) {
  df |>
    dplyr::filter(target_end_date >= max(target_end_date) - estimation_window) |>
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
    dplyr::mutate(
      count = confirm - dplyr::lag(confirm, default = 0L),
      .by = reference_date,
      .keep = "unused"
    )
}
