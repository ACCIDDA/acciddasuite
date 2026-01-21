# Rolling origin epidemic forecasts and model evaluation

Fits multiple time series models to weekly incidence data and evaluates
short term predictive performance using an expanding window rolling
origin scheme.

## Usage

``` r
get_fcast(df, eval_start_date, h = 4, top_n = 3, extra_models = NULL)
```

## Arguments

- df:

  Data frame of weekly observations containing `target_end_date` (Date)
  and `observation` (numeric).

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

An object of class `accida_cast` containing:

- forecast:

  Final `h` week ahead forecasts for all models and the ensemble.

- score:

  Model ranking based on rolling origin CRPS.

- plot:

  ggplot object showing forecasts and prediction intervals.

## Details

From `eval_start_date` onwards, models are repeatedly refitted on all
data available up to each evaluation time point and used to forecast the
next `h` weeks. Forecasts are compared to observations using the
Continuous Ranked Probability Score (CRPS).

Models are ranked by mean CRPS across evaluation periods. The best
performing `top_n` models are combined into an equal weight ensemble. A
final `h` week ahead forecast is then produced by refitting selected
models using the full dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
extra_models <- list(
  CUSTOM_ARIMA = ARIMA(observation ~ pdq(1, 1, 0)),
  PROPHET = fable.prophet::PROPHET(observation)
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
