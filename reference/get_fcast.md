# Forecast and evaluate time series models

Selects and evaluates multiple time series models via expanding-window
cross-validation, then produces a final forward-looking forecast.

## Usage

``` r
get_fcast(df, eval_start_date, h = 4, top_n = 3, extra_models = NULL)
```

## Arguments

- df:

  An `accidda_ncast` object from
  [`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md),
  or an `accidda_data` object from
  [`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
  or
  [`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md).
  If the underlying data frame contains `ncast_lower` and `ncast_upper`
  columns, nowcast uncertainty propagation is enabled automatically.

- eval_start_date:

  Date or string coercible to Date. First date at which forecasts are
  evaluated. At least 52 weeks of data must precede this date.

- h:

  Integer. Forecast horizon in weeks. Default is 4.

- top_n:

  Integer. Number of top ranked models included in the ensemble. Default
  is 3.

- extra_models:

  Named list of additional forecasting models passed to
  [`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).
  Each element must reference the `observation` column and be compatible
  with
  [`fabletools::forecast()`](https://generics.r-lib.org/reference/forecast.html).

  Custom models can be constructed using the fable modelling framework,
  see the [fabletools extension_models
  vignette](https://fabletools.tidyverts.org/articles/extension_models.html).

  The name of each list element is used as the model label in the
  output.

## Value

An object of class `accidda_fcast` containing:

- hubcast:

  Hub-format forecast object (`model_out_tbl` and `oracle_output`).

- score:

  Model ranking based on rolling origin WIS.

- plot:

  ggplot2 object showing forecasts and prediction intervals.

## Details

**Cross-validation.** From `eval_start_date` onwards, models are
repeatedly refitted on all data up to each cutoff and used to forecast
the next `h` weeks. Models are ranked by WIS; the best `top_n` form an
equal-weight ensemble.

**Nowcast uncertainty.** When an `accidda_ncast` object is supplied (or
an `accidda_data` whose data frame contains `ncast_lower` and
`ncast_upper` columns), cross-validation runs on the `observation`
column (the median corrected series). The final forecast is then
produced three times (from the median, lower 95\\ and the resulting
distributions are pooled, so prediction intervals reflect both model
uncertainty and nowcast uncertainty.

## Examples

``` r
if (FALSE) { # \dontrun{
extra_models <- list(
  CUSTOM_ARIMA = ARIMA(observation ~ pdq(1, 1, 0)),
  PROPHET = fable.prophet::PROPHET(observation),
  EPIESTIM = EPIESTIM(observation, mean_si = 3.96, std_si = 4.75)
)

get_fcast(
  df,
  eval_start_date = "2025-01-01",
  h = 4,
  top_n = 3,
  extra_models = extra_models
)
} # }
```
