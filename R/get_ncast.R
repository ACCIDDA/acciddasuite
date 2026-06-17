#' Nowcast right-truncated surveillance data
#'
#' Recent weeks of surveillance data are incomplete because of reporting
#' delays (right truncation). \code{get_ncast} estimates their final counts
#' with \href{https://baselinenowcast.epinowcast.org/}{baselinenowcast},
#' replacing the last \code{max_delay} weeks and leaving earlier weeks
#' untouched.
#'
#' Weekly data only; other cadences are rejected (the rest of the pipeline is
#' cadence-agnostic).
#'
#' @param x An \code{accidda_data} (\code{\link{get_data}} /
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
#' @return An \code{accidda_ncast} object with the shared backbone
#'   (\code{location}, \code{target}, \code{window}, \code{interval},
#'   \code{history}) plus:
#'   \describe{
#'     \item{data}{Corrected series. \code{observation} holds the nowcast
#'       median for corrected weeks; \code{ncast_lower} / \code{ncast_upper}
#'       (95\% CrI) are non-NA only there, and let \code{\link{get_fcast}}
#'       propagate nowcast uncertainty.}
#'     \item{plot}{ggplot of the correction.}
#'   }
#'
#' @examples
#' \dontrun{
#' x     <- get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
#' ncast <- get_ncast(x)
#' }
#'
#' @export
#' @importFrom dplyr transmute summarise filter select mutate group_by
#'   ungroup left_join coalesce arrange if_else
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs
#'   scale_fill_manual guide_legend theme_classic
#'
get_ncast <- function(
  x,
  max_delay = 2,
  draws = 1000,
  prop_delay = 0.5,
  scale_factor = 3
) {
  # Accept accidda_data; coerce plain data frames via check_data()
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

  meta <- accidda_meta(x)
  interval <- meta$interval

  # Nowcasting is weekly-only.
  if (interval != 7L) {
    stop(
      "get_ncast() currently supports weekly data only ",
      "(detected reporting interval = ",
      interval,
      " days).\n",
      "Aggregate the series to weekly before nowcasting, or skip the nowcast ",
      "and pass the data straight to get_cv() / get_fcast()."
    )
  }

  df <- x$data

  latest_date <- max(df$target_end_date)
  # Only a recent window is used for delay estimation.
  estimation_window <- scale_factor * max_delay * interval # days

  # Latest known observation per date, over the full history.
  best_obs <- df |>
    dplyr::group_by(target_end_date) |>
    dplyr::filter(as_of == max(as_of)) |>
    dplyr::ungroup() |>
    dplyr::select(target_end_date, location, target, observation)

  # Reporting triangle -> posterior nowcast draws -> summary (incl. 95% CrI)
  triangle <- build_reporting_triangle(df, latest_date, estimation_window)

  rep_tri <- baselinenowcast::as_reporting_triangle(
    triangle,
    delays_unit = "weeks"
  )
  rep_tri <- baselinenowcast::truncate_to_delay(rep_tri, max_delay = max_delay)
  draws_tbl <- baselinenowcast::baselinenowcast(
    rep_tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    draws = draws
  )

  ncast_summary <- draws_tbl |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(
      median = stats::median(pred_count),
      lower = stats::quantile(pred_count, 0.025),
      upper = stats::quantile(pred_count, 0.975),
      q25 = stats::quantile(pred_count, 0.25),
      q75 = stats::quantile(pred_count, 0.75),
      .groups = "drop"
    )

  # Replace only the last max_delay weeks (right-truncated) with the nowcast.
  out_df <- build_corrected_series(
    best_obs,
    ncast_summary,
    latest_date,
    max_delay,
    interval
  )

  p <- plot_ncast(ncast_summary, best_obs, latest_date, estimation_window, meta)

  new_accidda_ncast(
    data = out_df,
    location = meta$location,
    target = meta$target,
    window = c(
      from = min(out_df$target_end_date),
      to = max(out_df$target_end_date)
    ),
    interval = interval,
    history = TRUE,
    plot = p
  )
}


#' Round dates to ISO week so reporting delays are always integer weeks
#' @keywords internal
#' @noRd
week_floor <- function(dates) {
  as.Date(cut(dates, "week"))
}


#' Build the incremental reporting triangle for baselinenowcast
#'
#' Differences successive revisions per reference date into new reports per
#' period; negative revisions are clamped to 0 (with a warning).
#' @param df A data frame with revision history (`as_of`).
#' @param latest_date The most recent `target_end_date`.
#' @param estimation_window Width in days of the delay-estimation window.
#' @return A data frame of `reference_date`, `report_date`, `count`.
#' @keywords internal
#' @noRd
build_reporting_triangle <- function(df, latest_date, estimation_window) {
  recent <- dplyr::filter(
    df,
    target_end_date >= latest_date - estimation_window
  )

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

  dplyr::select(obs, reference_date, report_date, count)
}


#' Splice the nowcast into the last `max_delay` weeks of the series
#'
#' Corrected weeks take the nowcast median and 95\% CrI bounds
#' (\code{ncast_lower} / \code{ncast_upper}); other weeks keep NA bounds.
#' @param best_obs Latest observation per date (one row per `target_end_date`).
#' @param ncast_summary Per-week nowcast summary (median + 50\%/95\% CrI bounds).
#' @param latest_date The most recent `target_end_date`.
#' @inheritParams get_ncast
#' @param interval Reporting interval in days.
#' @return The corrected series data frame.
#' @keywords internal
#' @noRd
build_corrected_series <- function(
  best_obs,
  ncast_summary,
  latest_date,
  max_delay,
  interval
) {
  ncast_cutoff <- latest_date - max_delay * interval

  ncast_lookup <- data.frame(
    reference_date = ncast_summary$reference_date,
    ncast_median = ncast_summary$median,
    ncast_lower = ncast_summary$lower,
    ncast_upper = ncast_summary$upper
  )

  best_obs |>
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
}


#' Visualise the nowcast over the estimation window
#' @param ncast_summary Per-week nowcast summary (median + 50\%/95\% CrI bounds).
#' @param best_obs Latest observation per date (one row per `target_end_date`).
#' @param latest_date The most recent `target_end_date`.
#' @param estimation_window Width, in days, of the estimation window.
#' @param meta Metadata backbone (for axis/subtitle labels).
#' @return A ggplot object.
#' @keywords internal
#' @noRd
plot_ncast <- function(
  ncast_summary,
  best_obs,
  latest_date,
  estimation_window,
  meta
) {
  obs_window <- best_obs |>
    dplyr::filter(target_end_date >= latest_date - estimation_window) |>
    dplyr::mutate(reference_date = week_floor(target_end_date))

  ggplot2::ggplot(ncast_summary, ggplot2::aes(x = reference_date)) +
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
        override.aes = list(alpha = c(0.4, 0.2))
      )
    ) +
    ggplot2::labs(
      x = "Date",
      y = meta$target,
      subtitle = meta$location,
      fill = NULL
    ) +
    ggplot2::theme_classic()
}
