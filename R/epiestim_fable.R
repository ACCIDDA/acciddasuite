specials_epiestim <- fabletools::new_specials(
  xreg = function(...) {
    stop("`EPIESTIM()` does not support exogenous regressors.")
  }
)

#' EpiEstim model for fable
#'
#' Estimates the current reproduction number (Rt) using
#' [EpiEstim][EpiEstim::estimate_R] and simulates future incidence using
#' [projections][projections::project]. Works with any regular aggregation
#' period and integrates into the fable workflow via
#' [model()][fabletools::model].
#'
#' The aggregation period is detected automatically from the tsibble index.
#' Only the most recent data (the `rt_window` estimation period plus the serial
#' interval period) is passed to EpiEstim's expectation-maximisation
#' (EM) algorithm, which reconstructs daily incidence from the aggregated
#' counts before estimating Rt on rolling `rt_window`-day windows.
#' The most recent Rt estimate is then used to project forward via stochastic simulation.
#' Forecasts are returned as sample distributions (one per horizon period) drawn from `n_sim` epidemic paths.
#'
#' @param formula Response variable, e.g. `observation`. Exogenous regressors
#'   are not supported.
#' @param mean_si Mean serial interval in days.
#' @param std_si Standard deviation of the serial interval in days.
#' @param rt_window Sliding window width in days for Rt estimation (`dt_out`
#'   in EpiEstim). Controls how many recent days inform the current Rt.
#'   Smaller values track recent trends more closely; larger values give a
#'   smoother estimate. Defaults to 14 days (~2 weeks).
#' @param n_sim Number of simulation paths for the forecast distribution.
#' @param R_fix_within If `TRUE`, Rt is held constant within each simulated
#'   path (recommended for short horizons).
#'
#' @return A model definition for use inside [model()][fabletools::model].
#' @export
#' @importFrom fabletools new_model_class new_model_definition
#' @importFrom tsibble is_regular measured_vars index_var
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom projections project
#' @importFrom incidence as.incidence
#' @importFrom distributional dist_sample
EPIESTIM <- function(
  formula,
  mean_si,
  std_si,
  rt_window = 14L,
  n_sim = 100L,
  R_fix_within = TRUE
) {
  model_epiestim <- fabletools::new_model_class(
    "epiestim",
    train = train_epiestim,
    specials = specials_epiestim,
    check = function(.data) {
      if (!tsibble::is_regular(.data)) {
        stop("Data must be a regular tsibble (no implicit gaps).")
      }
    }
  )

  fabletools::new_model_definition(
    model_epiestim,
    {{ formula }},
    mean_si = mean_si,
    std_si = std_si,
    rt_window = as.integer(rt_window),
    n_sim = as.integer(n_sim),
    R_fix_within = R_fix_within
  )
}


train_epiestim <- function(
  .data,
  specials,
  mean_si,
  std_si,
  rt_window,
  n_sim,
  R_fix_within,
  ...
) {
  mv <- tsibble::measured_vars(.data)
  if (length(mv) != 1L) {
    stop("`EPIESTIM()` is a univariate model.")
  }

  counts <- .data[[mv]]
  n_obs_all <- length(counts)
  if (n_obs_all < 3L) {
    stop("Need at least 3 observations to estimate Rt.")
  }

  # Detect aggregation period in days from the tsibble index
  dates <- sort(as.Date(.data[[tsibble::index_var(.data)]]))
  dt_days <- as.integer(dates[2] - dates[1])

  # Truncate to recent data only: rt_window + SI tail buffer (mean + 4*SD).
  si_buffer <- as.integer(ceiling(mean_si + 4 * std_si))
  n_keep <- max(4L, ceiling((rt_window + si_buffer) / dt_days) + 2L)
  counts_recent <- tail(counts, min(n_keep, n_obs_all))
  n_recent <- length(counts_recent)

  # Drop the last period (potentially right-censored) before estimation
  counts_est <- counts_recent[-n_recent]

  # EpiEstim EM algorithm: reconstruct daily incidence from the aggregated
  # counts, then estimate Rt on trailing rt_window-day sliding windows.
  # iter = 100L ensures convergence on the small truncated window.
  Rt <- EpiEstim::estimate_R(
    incid = counts_est,
    dt = dt_days,
    dt_out = rt_window,
    recon_opt = "match",
    iter = 100L,
    method = "parametric_si",
    config = EpiEstim::make_config(list(mean_si = mean_si, std_si = std_si))
  )

  # Build incidence object from EM-reconstructed daily counts for projections
  daily_incid <- incidence::as.incidence(
    Rt$I,
    dates = as.Date(Rt$dates),
    interval = 1
  )

  structure(
    list(
      Rt = Rt,
      daily_incid = daily_incid,
      mean_si = mean_si,
      std_si = std_si,
      dt_days = dt_days,
      rt_window = rt_window,
      n_sim = n_sim,
      R_fix_within = R_fix_within,
      y_name = mv,
      n_obs = n_obs_all,
      last_date = max(dates)
    ),
    class = "model_epiestim"
  )
}


# ------------------------------------------------------------------------------
# fabletools S3 methods
# ------------------------------------------------------------------------------

#' @importFrom fabletools model_sum
#' @export
model_sum.model_epiestim <- function(x) {
  sprintf(
    "EpiEstim[si=%.2f\u00b1%.2f, w=%d, n=%d]",
    x$mean_si,
    x$std_si,
    x$rt_window,
    x$n_sim
  )
}

#' @importFrom fabletools report
#' @export
report.model_epiestim <- function(x, ...) {
  R_row <- x$Rt$R[nrow(x$Rt$R), ]
  cat("\n--- EpiEstim + Projections Model ---\n\n")
  cat(sprintf(
    "  Serial interval : mean = %.2f days, SD = %.2f days\n",
    x$mean_si,
    x$std_si
  ))
  cat(sprintf("  Rt window       : last %d days\n", x$rt_window))
  cat(sprintf("  Simulations     : %d\n", x$n_sim))
  cat(sprintf("  R fix within    : %s\n\n", x$R_fix_within))
  cat("  Current Rt estimate:\n")
  cat(sprintf("    Median Rt : %.3f\n", R_row[["Median(R)"]]))
  cat(sprintf(
    "    95%% CrI   : [%.3f, %.3f]\n",
    R_row[["Quantile.0.025(R)"]],
    R_row[["Quantile.0.975(R)"]]
  ))
  cat(sprintf(
    "\n  Training data   : %d observations (%d-day period), last date = %s\n",
    x$n_obs,
    x$dt_days,
    format(x$last_date)
  ))
}

#' @importFrom fabletools tidy
#' @export
tidy.model_epiestim <- function(x, ...) {
  R_row <- x$Rt$R[nrow(x$Rt$R), ]
  data.frame(
    term = c("Rt_median", "Rt_lower_95", "Rt_upper_95"),
    estimate = c(
      R_row[["Median(R)"]],
      R_row[["Quantile.0.025(R)"]],
      R_row[["Quantile.0.975(R)"]]
    )
  )
}

#' @importFrom fabletools glance
#' @export
glance.model_epiestim <- function(x, ...) {
  R_row <- x$Rt$R[nrow(x$Rt$R), ]
  data.frame(
    mean_si = x$mean_si,
    std_si = x$std_si,
    rt_window = x$rt_window,
    n_sim = x$n_sim,
    Rt_median = R_row[["Median(R)"]],
    Rt_lower_95 = R_row[["Quantile.0.025(R)"]],
    Rt_upper_95 = R_row[["Quantile.0.975(R)"]]
  )
}

#' @importFrom fabletools fitted
#' @export
fitted.model_epiestim <- function(object, ...) rep(NA_real_, object$n_obs)

#' @importFrom fabletools residuals
#' @export
residuals.model_epiestim <- function(object, ...) rep(NA_real_, object$n_obs)

#' @importFrom fabletools forecast
#' @export
forecast.model_epiestim <- function(object, new_data, specials = NULL, ...) {
  h <- NROW(new_data)
  R_median <- tail(na.omit(object$Rt$R[["Median(R)"]]), 1)
  si_distr <- object$Rt$si_distr[-1]

  proj <- projections::project(
    x = object$daily_incid,
    R = R_median,
    si = si_distr,
    n_sim = object$n_sim,
    R_fix_within = object$R_fix_within,
    n_days = object$dt_days * h
  )

  # Sum daily simulations into per-period totals (h periods x n_sim paths)
  proj_matrix <- as.matrix(proj)
  period_labels <- rep(seq_len(h), each = object$dt_days)
  period_proj <- t(vapply(
    seq_len(h),
    \(p) colSums(proj_matrix[period_labels == p, , drop = FALSE]),
    numeric(object$n_sim)
  ))

  distributional::dist_sample(lapply(seq_len(h), \(p) period_proj[p, ]))
}
