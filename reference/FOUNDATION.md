# Forecast with a pretrained ("foundation") time-series model

`FOUNDATION()` lets you use a large pretrained forecasting model inside
`fable`, next to ARIMA, ETS and the others. These models are
*zero-shot*: there is no training step. Fitting just stores your
history; at forecast time the model produces many possible future paths,
which the package turns into quantiles, ensembles and scores like any
other model (see
[`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md),
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)).

## Usage

``` r
FOUNDATION(
  formula,
  backend = c("chronos", "timesfm", "sundial", "moirai"),
  model_id = NULL,
  device = c("cpu", "cuda"),
  n_samples = 200L
)
```

## Arguments

- formula:

  The series to forecast, e.g. `observation`. For counts, wrap it in
  [`log()`](https://rdrr.io/r/base/Log.html) (e.g. `log(observation)`)
  and `fable` undoes the log for you. Extra predictor variables are not
  supported.

- backend:

  Which model to use: `"chronos"`, `"timesfm"`, `"sundial"` or
  `"moirai"`.

- model_id:

  Exact model to fetch from Hugging Face. `NULL` (default) uses the
  backend's default model listed above.

- device:

  `"cpu"` (default), or `"cuda"` to use a GPU.

- n_samples:

  Number of forecast draws (default 200).

## Value

A model definition to pass to
[`model`](https://fabletools.tidyverts.org/reference/model.html) (or to
the package's
[`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md) /
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)).

## Details

The models run in Python, set up for you - you only need the
`reticulate` package installed. The first forecast of a session installs
the Python pieces and downloads the model weights (a one-off that may
take a few minutes); after that everything is cached and forecasts are
quick.

Available models (`backend`):

- `"chronos"`:

  Amazon Chronos. Default `amazon/chronos-t5-small`.

- `"timesfm"`:

  Google TimesFM. Default `google/timesfm-2.5-200m-pytorch`.

- `"sundial"`:

  Tsinghua Sundial. Default `thuml/sundial-base-128m`.

- `"moirai"`:

  Salesforce Moirai. Default `Salesforce/moirai-1.1-R-small`.

Note: TimesFM and the Chronos-Bolt models report only a handful of
quantiles, so their most extreme prediction intervals are approximate.

## See also

[`default_models`](https://accidda.github.io/acciddasuite/reference/default_models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ncast <- get_data("covid", "ny", revisions = TRUE) |> get_ncast()
get_fcast(ncast, models = c(
  default_models(),
  list(
    CHRONOS = FOUNDATION(log(observation), "chronos"),
    MOIRAI  = FOUNDATION(log(observation), "moirai")
  )
))
} # }
```
