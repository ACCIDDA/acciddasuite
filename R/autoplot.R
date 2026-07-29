#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' Plot surveillance data
#'
#' Observed counts over time, one panel per series. When revision history is
#' present, the latest reported value per week is shown.
#'
#' @param object An \code{incast_data} from \code{\link{check_data}}.
#' @param ... Ignored.
#' @return A ggplot object.
#' @export
autoplot.incast_data <- function(object, ...) {
  m <- incast_meta(object)
  ggplot2::ggplot(
    extract_series(object),
    ggplot2::aes(target_end_date, observation)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(m$key, scales = "free_y") +
    ggplot2::labs(x = "Date", y = m$target) +
    ggplot2::theme_classic()
}


#' Median line with 50\% (dark) and 95\% (light) bands, one panel per series
#' @param bands A data frame with `lower`, `q25`, `median`, `q75`, `upper`.
#' @param x The date column, unquoted.
#' @param key Key column name(s), one facet per series.
#' @param target Y-axis label.
#' @keywords internal
#' @noRd
plot_bands <- function(bands, x, key, target) {
  ggplot2::ggplot(bands, ggplot2::aes({{ x }})) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75), alpha = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = median)) +
    ggplot2::facet_wrap(key, scales = "free_y") +
    ggplot2::labs(x = "Date", y = target) +
    ggplot2::theme_classic()
}


#' Plot a nowcast
#'
#' Plot reported counts alongside nowcast distributions. Nowcasts are shown as
#' the median, with 50% and 95% credible intervals.
#'
#' Each panel corresponds to a single time series.
#'
#' @param object An \code{incast_ncast} object returned by
#' \code{\link{get_ncast}}.
#' @param ... Ignored.
#'
#' @return A ggplot object.
#' @export
autoplot.incast_ncast <- function(object, ...) {
  m <- incast_meta(object)
  plot_bands(
    object$meta$ncast_summary,
    reference_date,
    m$key,
    m$target
  ) +
    ggplot2::geom_point(ggplot2::aes(y = observed), size = 0.7)
}


#' Plot cross-validation model rankings
#'
#' Plot model performance across time series using the weighted interval score
#' (WIS). By default, models are ranked using relative WIS, where values below
#' 1 indicate better-than-average performance and values above 1 indicate
#' worse-than-average performance. If only one model is present, raw WIS
#' values are displayed instead.
#'
#' Each panel corresponds to a single time series.
#'
#'
#' @param object An \code{incast_cv} object returned by \code{\link{get_cv}}.
#' @param ... Ignored.
#'
#' @return A ggplot object.
#' @export
autoplot.incast_cv <- function(object, ...) {
  m <- incast_meta(object)
  score <- object$score

  p <- if ("wis_relative_skill" %in% names(score)) {
    ggplot2::ggplot(
      score,
      ggplot2::aes(
        wis_relative_skill,
        stats::reorder(model_id, -wis_relative_skill)
      )
    ) +
      ggplot2::geom_vline(xintercept = 1, colour = "grey70") +
      ggplot2::geom_segment(ggplot2::aes(xend = 1)) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::facet_wrap(m$key) +
      ggplot2::labs(x = "Relative WIS")
  } else {
    ggplot2::ggplot(score, ggplot2::aes(wis, model_id)) +
      ggplot2::geom_segment(ggplot2::aes(xend = 0)) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::facet_wrap(m$key, scales = "free_x") +
      ggplot2::labs(x = "WIS")
  }

  p + ggplot2::labs(y = "Model") + ggplot2::theme_classic()
}


#' Plot a forecast
#'
#' Plot observed values alongside forecast distributions for a selected model.
#' Forecasts are shown as the median, with 50% and 95% prediction intervals.
#' By default, the ensemble forecast is displayed.
#'
#' Each panel corresponds to a single time series.
#'
#' @param object An \code{incast_fcast} object returned by
#' \code{\link{get_fcast}}.
#' @param model The model to plot. Defaults to \code{"ENSEMBLE"}.
#' @param ... Ignored.
#'
#' @return A ggplot object.
#' @export
autoplot.incast_fcast <- function(object, model = "ENSEMBLE", ...) {
  m <- incast_meta(object)
  out <- object$hub$model_out_tbl
  if (length(model) != 1L || !model %in% out$model_id) {
    stop("`model` must be one of: ", paste(unique(out$model_id), collapse = ", "))
  }

  bands <- out |>
    dplyr::filter(model_id == model) |>
    tidyr::pivot_wider(names_from = output_type_id, values_from = value) |>
    dplyr::rename(
      lower = "0.025", q25 = "0.25", median = "0.5",
      q75 = "0.75", upper = "0.975"
    )

  plot_bands(bands, target_end_date, m$key, m$target) +
    ggplot2::geom_line(
      data = object$hub$oracle_output,
      ggplot2::aes(y = oracle_value)
    )
}
