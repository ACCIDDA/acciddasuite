specials_foundation <- fabletools::new_specials(
  xreg = function(...) {
    stop("`FOUNDATION()` does not support exogenous regressors.")
  }
)

#' Forecast with a pretrained ("foundation") time-series model
#'
#' \code{FOUNDATION()} lets you use a large pretrained forecasting model inside
#' \code{fable}, next to ARIMA, ETS and the others. These models are
#' \emph{zero-shot}: there is no training step. Fitting just stores your
#' history; at forecast time the model produces many possible future paths,
#' which the package turns into quantiles, ensembles and scores like any other
#' model (see \code{\link{get_cv}}, \code{\link{get_fcast}}).
#'
#' The models run in Python, set up for you - you only need the
#' \code{reticulate} package installed. The first forecast of a session installs
#' the Python pieces and downloads the model weights (a one-off that may take a
#' few minutes); after that everything is cached and forecasts are quick.
#'
#' Available models (\code{backend}):
#' \describe{
#'   \item{\code{"chronos"}}{Amazon Chronos. Default
#'     \code{amazon/chronos-t5-small}.}
#'   \item{\code{"timesfm"}}{Google TimesFM. Default
#'     \code{google/timesfm-2.5-200m-pytorch}.}
#'   \item{\code{"sundial"}}{Tsinghua Sundial. Default
#'     \code{thuml/sundial-base-128m}.}
#'   \item{\code{"moirai"}}{Salesforce Moirai. Default
#'     \code{Salesforce/moirai-1.1-R-small}.}
#' }
#'
#' Note: TimesFM and the Chronos-Bolt models report only a handful of quantiles,
#' so their most extreme prediction intervals are approximate.
#'
#' @param formula The series to forecast, e.g. \code{observation}. For counts,
#'   wrap it in \code{log()} (e.g. \code{log(observation)}) and \code{fable}
#'   undoes the log for you. Extra predictor variables are not supported.
#' @param backend Which model to use: \code{"chronos"}, \code{"timesfm"},
#'   \code{"sundial"} or \code{"moirai"}.
#' @param model_id Exact model to fetch from Hugging Face. \code{NULL} (default)
#'   uses the backend's default model listed above.
#' @param device \code{"cpu"} (default), or \code{"cuda"} to use a GPU.
#' @param n_samples Number of forecast draws (default 200).
#'
#' @return A model definition to pass to \code{\link[fabletools]{model}} (or to
#'   the package's \code{\link{get_cv}} / \code{\link{get_fcast}}).
#'
#' @examples
#' \dontrun{
#' ncast <- get_data("covid", "ny", revisions = TRUE) |> get_ncast()
#' get_fcast(ncast, models = c(
#'   default_models(),
#'   list(
#'     CHRONOS = FOUNDATION(log(observation), "chronos"),
#'     MOIRAI  = FOUNDATION(log(observation), "moirai")
#'   )
#' ))
#' }
#'
#' @seealso \code{\link{default_models}}
#' @export
#' @importFrom fabletools new_model_class new_model_definition
#' @importFrom tsibble is_regular measured_vars
#' @importFrom distributional dist_sample
FOUNDATION <- function(
  formula,
  backend = c("chronos", "timesfm", "sundial", "moirai"),
  model_id = NULL,
  device = c("cpu", "cuda"),
  n_samples = 200L
) {
  backend <- match.arg(backend)
  device <- match.arg(device)
  spec <- foundation_backend(backend)
  if (is.null(model_id)) {
    model_id <- spec$default_model
  }
  if (
    !is.numeric(n_samples) ||
      length(n_samples) != 1L ||
      n_samples < 2 ||
      n_samples != round(n_samples)
  ) {
    stop("`n_samples` must be a single integer >= 2.")
  }

  # Declare this backend's Python deps now, before any forecast starts the Python
  # session, so a run mixing backends resolves one environment that satisfies all
  # of them (e.g. sundial's transformers pin is set before chronos imports it).
  ensure_reticulate()
  reticulate::py_require(spec$deps)

  model_foundation <- fabletools::new_model_class(
    "foundation",
    train = train_foundation,
    specials = specials_foundation,
    check = function(.data) {
      if (!tsibble::is_regular(.data)) {
        stop("Data must be a regular tsibble (no implicit gaps).")
      }
    }
  )

  fabletools::new_model_definition(
    model_foundation,
    {{ formula }},
    backend = backend,
    model_id = model_id,
    device = device,
    n_samples = as.integer(n_samples)
  )
}


train_foundation <- function(
  .data,
  specials,
  backend,
  model_id,
  device,
  n_samples,
  ...
) {
  mv <- tsibble::measured_vars(.data)
  if (length(mv) != 1L) {
    stop("`FOUNDATION()` is a univariate model.")
  }

  # Zero-shot: keep the history as context; the pretrained model is only called
  # at forecast time (see forecast.model_foundation).
  y <- as.numeric(.data[[mv]])
  if (sum(!is.na(y)) < 2L) {
    stop(
      "Need at least 2 non-missing observations to forecast with FOUNDATION()."
    )
  }

  structure(
    list(
      context = y,
      backend = backend,
      model_id = model_id,
      device = device,
      n_samples = n_samples,
      n_obs = length(y)
    ),
    class = "model_foundation"
  )
}


# ------------------------------------------------------------------------------
# fabletools S3 methods
# ------------------------------------------------------------------------------

#' @importFrom fabletools model_sum
#' @export
model_sum.model_foundation <- function(x) {
  sprintf("%s[%s]", toupper(x$backend), basename(x$model_id))
}

#' @importFrom fabletools report
#' @export
report.model_foundation <- function(object, ...) {
  cat("\n--- Foundation model (zero-shot) ---\n\n")
  cat(sprintf("  Backend      : %s\n", object$backend))
  cat(sprintf("  Model id     : %s\n", object$model_id))
  cat(sprintf("  Device       : %s\n", object$device))
  cat(sprintf("  Draws        : %d\n", object$n_samples))
  cat(sprintf("  Context      : %d observations\n\n", object$n_obs))
  invisible(object)
}

#' @importFrom fabletools glance
#' @export
glance.model_foundation <- function(x, ...) {
  data.frame(
    backend = x$backend,
    model_id = x$model_id,
    device = x$device,
    n_samples = x$n_samples,
    n_obs = x$n_obs
  )
}

#' @importFrom stats fitted
#' @export
fitted.model_foundation <- function(object, ...) rep(NA_real_, object$n_obs)

#' @importFrom stats residuals
#' @export
residuals.model_foundation <- function(object, ...) rep(NA_real_, object$n_obs)

#' @importFrom fabletools forecast
#' @export
forecast.model_foundation <- function(object, new_data, specials = NULL, ...) {
  h <- NROW(new_data)

  paths <- foundation_sample_paths(
    backend = object$backend,
    context = object$context,
    h = h,
    model_id = object$model_id,
    device = object$device,
    n_samples = object$n_samples
  )

  # Draws as a sample distribution: flows through the package's truncation,
  # mixture and WIS scoring, and fable back-transforms them (e.g. inverts log()).
  distributional::dist_sample(lapply(seq_len(h), function(i) paths[i, ]))
}
