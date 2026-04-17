specials_lemma <- fabletools::new_specials(
  xreg = function(...) {
    stop("`LEMMA()` does not support exogenous regressors.")
  }
)

#' LEMMA-forecast model for fable
#'
#' Fits [LEMMA-forecast](https://github.com/ACCIDDA/LEMMA-forecast) through
#' the fable interface. Training stores the series and settings; `forecast()`
#' writes the series to a Hubverse-shaped CSV, runs LEMMA's `main.py` in a
#' dedicated Python virtualenv (via a subprocess), reads LEMMA's quantile
#' output back, and turns it into a sample-based `distributional` object
#' compatible with `fabletools::forecast()` and the mixture ensembles built
#' by [get_fcast()].
#'
#' Requires [install_lemma()] to have been run once to set up the
#' LEMMA-forecast clone and the Python dependencies.
#'
#' @param formula Response variable, e.g. `observation`. Exogenous
#'   regressors are not supported.
#' @param approach Predictor approach name as understood by LEMMA
#'   (`"Flatline"`, `"ARIMA"`, `"SIKJalpha"`, `"VAR_UN"`).
#' @param ensemble_method Ensemble aggregator (`"Random Forest"` by
#'   default; see LEMMA's `config_model` for alternatives).
#' @param quantiles Output quantile levels passed to LEMMA. Must lie in
#'   `[0, 1]`. Finer grids give better sample reconstruction.
#' @param timesteps_per_bin Number of base timesteps per forecast bin.
#'   If `NULL` (the default), it is auto-detected from the tsibble index
#'   spacing (e.g. 7 for weekly data, 1 for daily data).
#' @param reference_date Reference date used internally by LEMMA for date
#'   arithmetic. `NULL` keeps LEMMA's default. Accepts `Date` or strings
#'   coercible to `Date`.
#' @param season_start_date Season start date, used by `"SIKJalpha"` to
#'   initialise its mechanistic component. `NULL` keeps LEMMA's default.
#'   Accepts `Date` or strings coercible to `Date`.
#' @param hyperparams Named list of approach-specific hyperparameters,
#'   forwarded verbatim to the generated `user_config.py`. Entries override
#'   LEMMA's defaults. See LEMMA's `user_config.py` for the full list
#'   (e.g. `arima_autoregressive_orders`, `flatline_lag_timesteps`,
#'   `halpha_list`, `un_list`, `hk`, `hjp`).
#' @param n_sim Number of samples drawn from LEMMA's quantile grid per
#'   horizon to build the `dist_sample` distribution returned from
#'   `forecast()`.
#'
#' @return A model definition for use inside [model()][fabletools::model].
#' @export
#' @importFrom fabletools new_model_class new_model_definition
#' @importFrom tsibble is_regular measured_vars index_var
LEMMA <- function(
  formula,
  approach = "Flatline",
  ensemble_method = "Random Forest",
  quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
  timesteps_per_bin = NULL,
  reference_date = NULL,
  season_start_date = NULL,
  hyperparams = list(),
  n_sim = 1000L
) {
  if (!is.list(hyperparams)) {
    stop("`hyperparams` must be a named list.", call. = FALSE)
  }
  if (length(hyperparams) > 0L && is.null(names(hyperparams))) {
    stop("`hyperparams` must be a named list.", call. = FALSE)
  }

  model_lemma <- fabletools::new_model_class(
    "lemma",
    train = train_lemma,
    specials = specials_lemma,
    check = function(.data) {
      if (!tsibble::is_regular(.data)) {
        stop("Data must be a regular tsibble (no implicit gaps).")
      }
    }
  )

  fabletools::new_model_definition(
    model_lemma,
    {{ formula }},
    approach = approach,
    ensemble_method = ensemble_method,
    quantiles = quantiles,
    timesteps_per_bin = timesteps_per_bin,
    reference_date = reference_date,
    season_start_date = season_start_date,
    hyperparams = hyperparams,
    n_sim = as.integer(n_sim)
  )
}


train_lemma <- function(
  .data,
  specials,
  approach,
  ensemble_method,
  quantiles,
  timesteps_per_bin,
  reference_date,
  season_start_date,
  hyperparams,
  n_sim,
  ...
) {
  mv <- tsibble::measured_vars(.data)
  if (length(mv) != 1L) {
    stop("`LEMMA()` is a univariate model.")
  }

  dates <- sort(as.Date(.data[[tsibble::index_var(.data)]]))
  counts <- .data[[mv]]

  # Auto-detect the tsibble spacing (1 = daily, 7 = weekly, ...)
  if (is.null(timesteps_per_bin)) {
    if (length(dates) < 2L) {
      stop(
        "Cannot auto-detect `timesteps_per_bin` from fewer than 2 observations.",
        call. = FALSE
      )
    }
    timesteps_per_bin <- as.integer(dates[2] - dates[1])
  }

  structure(
    list(
      dates = dates,
      counts = counts,
      approach = approach,
      ensemble_method = ensemble_method,
      quantiles = quantiles,
      timesteps_per_bin = as.integer(timesteps_per_bin),
      reference_date = reference_date,
      season_start_date = season_start_date,
      hyperparams = hyperparams,
      n_sim = n_sim,
      y_name = mv,
      n_obs = length(counts),
      last_date = max(dates)
    ),
    class = "model_lemma"
  )
}


# ------------------------------------------------------------------------------
# fabletools S3 methods
# ------------------------------------------------------------------------------

#' @importFrom fabletools model_sum
#' @export
model_sum.model_lemma <- function(x) {
  sprintf("LEMMA[%s/%s]", x$approach, x$ensemble_method)
}

#' @importFrom fabletools report
#' @export
report.model_lemma <- function(object, ...) {
  cat("\n--- LEMMA-forecast Model ---\n\n")
  cat(sprintf("  Approach            : %s\n", object$approach))
  cat(sprintf("  Ensemble method     : %s\n", object$ensemble_method))
  cat(sprintf(
    "  Quantiles           : %s\n",
    paste(object$quantiles, collapse = ", ")
  ))
  cat(sprintf("  Timesteps per bin   : %d\n", object$timesteps_per_bin))
  cat(sprintf(
    "  Samples (n_sim)     : %d\n",
    object$n_sim
  ))
  if (length(object$hyperparams) > 0L) {
    cat(sprintf(
      "  Custom hyperparams  : %s\n",
      paste(names(object$hyperparams), collapse = ", ")
    ))
  }
  cat(sprintf(
    "\n  Training data       : %d observations, last date = %s\n",
    object$n_obs,
    format(object$last_date)
  ))
}

#' @importFrom fabletools tidy
#' @export
tidy.model_lemma <- function(x, ...) {
  data.frame(
    term = c("approach", "ensemble_method"),
    estimate = c(x$approach, x$ensemble_method)
  )
}

#' @importFrom fabletools glance
#' @export
glance.model_lemma <- function(x, ...) {
  data.frame(
    approach = x$approach,
    ensemble_method = x$ensemble_method,
    timesteps_per_bin = x$timesteps_per_bin,
    n_sim = x$n_sim
  )
}

#' @importFrom stats fitted
#' @export
fitted.model_lemma <- function(object, ...) rep(NA_real_, object$n_obs)

#' @importFrom stats residuals
#' @export
residuals.model_lemma <- function(object, ...) rep(NA_real_, object$n_obs)


#' @importFrom fabletools forecast
#' @importFrom distributional dist_sample
#' @importFrom utils write.csv read.csv
#' @importFrom stats approx runif
#' @export
forecast.model_lemma <- function(object, new_data, specials = NULL, ...) {
  h <- NROW(new_data)
  lemma_path <- .lemma_path()

  # Per-call temp workspace (auto-cleaned on exit).
  work_dir <- tempfile("lemma_run_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  input_csv <- file.path(work_dir, "input.csv")
  output_csv <- file.path(work_dir, "output.csv")

  # 1. Write the training series in Hubverse shape for LEMMA to ingest.
  utils::write.csv(
    data.frame(
      location = "series",
      target_end_date = object$dates,
      observation = object$counts
    ),
    input_csv,
    row.names = FALSE
  )

  # 2. Write a temporary user_config.py with this call's parameters.
  writeLines(
    .lemma_user_config(
      input_csv = input_csv,
      output_csv = output_csv,
      bins_ahead = h,
      timesteps_per_bin = object$timesteps_per_bin,
      approach = object$approach,
      ensemble_method = object$ensemble_method,
      quantiles = object$quantiles,
      reference_date = object$reference_date,
      season_start_date = object$season_start_date,
      hyperparams = object$hyperparams
    ),
    file.path(work_dir, "user_config.py")
  )

  # 3. Write a tiny runner script that:
  #      - puts work_dir ahead of lemma_path on sys.path, so Python picks
  #        up OUR user_config.py instead of the one shipped with LEMMA,
  #      - chdirs into lemma_path so relative imports / data paths work,
  #      - executes main.py the same way `python main.py` would.
  runner <- file.path(work_dir, "_runner.py")
  writeLines(
    c(
      "import sys, os",
      sprintf("sys.path.insert(0, r'%s')", lemma_path),
      sprintf("sys.path.insert(0, r'%s')", work_dir),
      sprintf("os.chdir(r'%s')", lemma_path),
      "import runpy",
      "runpy.run_path('main.py', run_name='__main__')"
    ),
    runner
  )

  # 4. Call the venv's Python as a subprocess.
  python <- reticulate::virtualenv_python("acciddasuite-lemma")
  out <- system2(
    python,
    args = runner,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(out, "status")
  if ((!is.null(status) && status != 0L) || !file.exists(output_csv)) {
    stop(
      "LEMMA run failed:\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }

  # 5. Parse the quantile output and convert to a sample distribution so
  #    it mixes cleanly with other fable models inside `get_fcast()`.
  df <- utils::read.csv(output_csv, stringsAsFactors = FALSE)
  df <- df[df$output_type == "quantile", , drop = FALSE]
  df$horizon <- as.integer(df$horizon)
  df$output_type_id <- as.numeric(df$output_type_id)
  df$value <- as.numeric(df$value)

  horizons <- sort(unique(df$horizon))
  if (length(horizons) != h) {
    stop(
      sprintf(
        "LEMMA returned %d horizons, expected %d.",
        length(horizons),
        h
      ),
      call. = FALSE
    )
  }

  samples <- lapply(horizons, function(hz) {
    sub <- df[df$horizon == hz, ]
    o <- order(sub$output_type_id)
    # Invert LEMMA's piecewise-linear quantile function at uniform u's
    stats::approx(
      x = sub$output_type_id[o],
      y = sub$value[o],
      xout = stats::runif(object$n_sim),
      rule = 2L
    )$y
  })

  distributional::dist_sample(samples)
}


# ------------------------------------------------------------------------------
# Internal: generate a user_config.py for a single LEMMA run.
# Merges sensible defaults with user-supplied hyperparameters. Only settings
# we actively set are written; config_param.py already supplies defaults for
# anything omitted, via getattr().
# ------------------------------------------------------------------------------
.lemma_user_config <- function(
  input_csv,
  output_csv,
  bins_ahead,
  timesteps_per_bin,
  approach,
  ensemble_method,
  quantiles,
  reference_date = NULL,
  season_start_date = NULL,
  hyperparams = list()
) {
  # LEMMA's shipped hyperparameter defaults, emitted as verbatim Python source
  # so the numpy semantics (arrays vs. scalars) match LEMMA's expectations
  # exactly. `config_param.py` calls `len()` on several of these, so single-
  # value defaults must be emitted as `np.array([x])`, never as bare scalars.
  # Users override individual entries via the `hyperparams` argument; those
  # values go through .py_repr() below.
  hp_py <- c(
    arima_autoregressive_orders = "np.array([7.0, 14.0])",
    arima_differencing_orders   = "np.array([0.0, 1.0, 2.0])",
    flatline_lag_timesteps      = "np.array([0.0, 7.0, 14.0])",
    rlags                       = "np.array([0])",
    retro_lag_bins              = "np.array([1.0])",
    un_list                     = "np.array([50.0])",
    halpha_list                 = "np.arange(0.98, 0.92, -0.02)",
    S                           = "np.array([0.0])",
    hk                          = "2",
    hjp                         = "7"
  )
  for (nm in names(hyperparams)) {
    hp_py[[nm]] <- .py_repr(hyperparams[[nm]])
  }
  hp_lines <- sprintf("%s = %s", names(hp_py), unname(hp_py))

  # Dates: keep LEMMA's hard-coded defaults unless the user supplied something.
  ref_py <- if (is.null(reference_date)) {
    "datetime(2021, 9, 1)"
  } else {
    .py_date(reference_date)
  }
  season_py <- if (is.null(season_start_date)) {
    "datetime(2023, 9, 30)"
  } else {
    .py_date(season_start_date)
  }

  c(
    "from datetime import datetime",
    "import numpy as np",
    "",
    "# --- Acciddasuite-generated config (one-shot) ---",
    "base_time_step_unit = 'day'",
    "forecast_bin_unit = 'week'",
    sprintf("timesteps_per_bin = %d", as.integer(timesteps_per_bin)),
    sprintf("bins_ahead = %d", as.integer(bins_ahead)),
    "smoothing_window_timesteps = 7",
    sprintf("reference_date = %s", ref_py),
    sprintf("season_start_date = %s", season_py),
    sprintf("predictor_approach = '%s'", approach),
    sprintf("ensemble_method = '%s'", ensemble_method),
    sprintf("quantiles = %s", .py_repr(quantiles)),
    "target_data_path = None",
    "location_metadata_path = None",
    sprintf("hubverse_input_path = r'%s'", input_csv),
    sprintf("hubvsereInput = r'%s'", input_csv),
    "hubverse_target = None",
    "training_window_start_date = None",
    "training_window_end_date = None",
    "forecast_window_start_date = None",
    "forecast_window_end_date = None",
    sprintf("forecast_output_path = r'%s'", output_csv),
    "forecast_output_format = 'csv'",
    "",
    "# --- Approach hyperparameters (LEMMA defaults + user overrides) ---",
    hp_lines
  )
}


# ------------------------------------------------------------------------------
# Internal: serialise an R value to a Python literal suitable for emission
# into the generated user_config.py.
#
#   NULL                        -> None
#   Date / POSIXt               -> datetime(Y, M, D)
#   scalar integer              -> int literal
#   scalar double               -> float literal
#   scalar character            -> 'quoted' literal
#   scalar logical              -> True / False
#   numeric vector (length > 1) -> np.array([...])
#   character vector (len > 1)  -> ['x', 'y']
#   logical vector (len > 1)    -> [True, False]
#   value wrapped in I()        -> forced np.array([...]) regardless of length
#                                  (use when LEMMA calls len() on a length-1
#                                  override, e.g. `rlags = I(0L)`)
# ------------------------------------------------------------------------------
.py_repr <- function(x) {
  if (is.null(x)) return("None")
  if (inherits(x, c("Date", "POSIXt"))) return(.py_date(x))

  # I()-wrapped numerics are always emitted as arrays.
  forced_array <- inherits(x, "AsIs") && is.numeric(x)
  if (forced_array) x <- unclass(x)

  if (is.logical(x)) {
    tokens <- ifelse(x, "True", "False")
    if (length(x) == 1L) return(tokens)
    return(sprintf("[%s]", paste(tokens, collapse = ", ")))
  }

  if (is.character(x)) {
    tokens <- sprintf("'%s'", gsub("'", "\\\\'", x))
    if (length(x) == 1L) return(tokens)
    return(sprintf("[%s]", paste(tokens, collapse = ", ")))
  }

  if (is.numeric(x)) {
    if (length(x) == 1L && !forced_array) {
      if (is.integer(x)) return(format(x, scientific = FALSE))
      return(format(x, scientific = FALSE, nsmall = 1L, trim = TRUE))
    }
    tokens <- format(x, scientific = FALSE, trim = TRUE)
    return(sprintf("np.array([%s])", paste(tokens, collapse = ", ")))
  }

  stop(
    sprintf("Cannot serialise R value of class '%s' to Python.", class(x)[1]),
    call. = FALSE
  )
}


.py_date <- function(x) {
  d <- as.Date(x)
  if (length(d) != 1L || is.na(d)) {
    stop("Dates passed to LEMMA must be a single, non-NA Date.", call. = FALSE)
  }
  sprintf(
    "datetime(%d, %d, %d)",
    as.integer(format(d, "%Y")),
    as.integer(format(d, "%m")),
    as.integer(format(d, "%d"))
  )
}
