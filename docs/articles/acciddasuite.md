# acciddasuite

## Overview

`acciddasuite` builds infectious disease forecasts in a few steps:

1.  **[`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md)**
    or
    **[`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md)**:
    fetch or validate surveillance data.
2.  **[`get_ncast()`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)**
    *(optional)*: correct recent weeks for reporting delays.
3.  **[`get_cv()`](https://accidda.github.io/acciddasuite/reference/get_cv.md)**
    *(optional)*: evaluate candidate models by time series
    cross-validation.
4.  **[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)**:
    ensemble the best models into a forward-looking forecast.

The package relies on the [`fable`](https://fable.tidyverts.org/)
modeling framework and follows the standard forecasting workflow
described by [Hyndman & Athanasopoulos
(2021)](https://otexts.com/fpp3/basic-steps.html). The overall goal is
to provide public health professionals with an easily-adoptable approach
to generating, evaluating forecasts, and visualizing infectious disease
forecasts.

To get more information about how to know whether forecasting is the
best approach for your task, follow the steps in
[this](https://accidda.github.io/acciddasuite/articles/forecast_planning.md)
article.

## Step 1: Get data

We fetch weekly COVID-19 hospital admissions for New York from the [CDC
NHSN](https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/mpgq-jmmr/about_data)
via [`epidatr`](https://cmu-delphi.github.io/epidatr/).

Setting `revisions = TRUE` retrieves the full revision history (*i.e.*
all past versions of the data), which is needed for nowcasting.

[`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md)
returns a validated `accidda_data` object:

``` r

library(acciddasuite)
df <- get_data(pathogen = "covid", geo_value = "ny", revisions = TRUE)
df
#> <accidda_data>
#> Target:   wk inc covid hosp
#> Series:   1 (location)
#> Window:   2020-08-08 to 2026-07-18 (7-day interval)
#> History:  2024-11-17 to 2026-07-19
```

You can also **bring your own data**. Just pass it through
[`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md).
See
[`vignette("external_data")`](https://accidda.github.io/acciddasuite/articles/external_data.md)
for formatting details.

## Step 2: Nowcasting (optional)

The most recent weeks of surveillance data are almost always too low
because hospitals are still filing late reports (**right truncated**).
If you feed these raw counts into a forecaster, predictions will be
biased downward.

[`get_ncast()`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)
estimates what the recent counts will look like once all reports arrive.
With the default `max_delay = 2`, the last 2 weeks are corrected;
everything before that is left untouched.

``` r

ncast <- get_ncast(df, max_delay = 3)
ncast
#> <accidda_ncast>
#> Target:   wk inc covid hosp
#> Series:   1 (location)
#> Window:   2020-08-08 to 2026-07-18 (7-day interval)
#> Nowcast:  2026-07-04 to 2026-07-18
```

``` r

autoplot(ncast)
```

![](acciddasuite_files/figure-html/plot-nowcast-1.png)

The corrected `ncast$data` contains two extra columns: `ncast_lower` and
`ncast_upper` (95% CrI) for the corrected weeks.
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
detects these automatically and uses them to propagate nowcasting
uncertainty into the final forecast.

## Step 3: Forecasting

Forecasting is split into two steps:

1.  **[`get_cv()`](https://accidda.github.io/acciddasuite/reference/get_cv.md)
    (model selection)**: time series cross-validation on the full
    (median corrected) series, starting from `eval_start_date`. Models
    are ranked by WIS and interval coverage.
2.  **[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
    (final forecast)**: reuses the ranking to ensemble the best `top_n`
    models and projects `h` weeks into the future. When nowcast columns
    are present, the forecast is produced from three baselines (lower,
    median, and upper nowcast estimates) and pooled, so prediction
    intervals reflect both model uncertainty and nowcast uncertainty.

We set `eval_start_date` to mark the start of the evaluation window. All
observations before this date form the initial training window, so every
series needs at least two observations before it.

``` r

eval_start_date <- max(ncast$data$target_end_date) - 28
```

Default models are:

- `NAIVE` (Naïve / random walk): Carries the last observed value
  forward. The simplest possible baseline.

- `ETS` (Exponential Smoothing): A weighted average where recent weeks
  matter more than older ones. Adapts to trends and seasonal patterns.

- `THETA`: Splits the data into a long-term trend and short-term
  fluctuations, forecasts each separately, then combines them.

- `ARIMA`: Learns repeating patterns from past values to predict future
  ones. Auto-configured to find the best fit.

``` r

cv <- get_cv(
  ncast,
  eval_start_date = eval_start_date,
  h = 4
)
cv
#> <accidda_cv>
#> Target:   wk inc covid hosp
#> Series:   1 (location)
#> Window:   2020-08-08 to 2026-07-18 (7-day interval)
#> CV:       4 models x 1 origins (h = 4)
```

``` r

fcast <- get_fcast(cv, top_n = 3)
fcast
#> <accidda_fcast>
#> Target:   wk inc covid hosp
#> Series:   1 (location)
#> Forecast: 2026-07-25 to 2026-08-15 (h = 4)
#> Models:   3 + ENSEMBLE
```

Plot the ensemble forecast with
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
(pass `model =` to inspect any single model instead):

``` r

autoplot(fcast)
```

![](acciddasuite_files/figure-html/plot-forecast-1.png)

### Adding custom models

Any model compatible with the [`fable`](https://fable.tidyverts.org/)
framework can be passed to
[`get_cv()`](https://accidda.github.io/acciddasuite/reference/get_cv.md)
via `models`. Compose with
[`default_models()`](https://accidda.github.io/acciddasuite/reference/default_models.md)
to keep the built-ins alongside your own:

``` r

library(fable)
library(fable.prophet)
library(EpiEstim)
library(projections)
my_models <- c(
  default_models(),
  list(
    CUSTOM_ARIMA = ARIMA(observation ~ pdq(1, 1, 0)),
    PROPHET = prophet(observation ~ season("year")),
    EPIESTIM = EPIESTIM(observation, mean_si = 3, std_si = 2, rt_window = 7)
  )
)

cv <- get_cv(
  ncast,
  eval_start_date = eval_start_date,
  h = 3,
  models = my_models
)

fcast <- get_fcast(cv, top_n = 3)
```

## Submit to RespiLens

[RespiLens](https://www.respilens.com/) is a platform for sharing
respiratory disease forecasts. Use
[`to_respilens()`](https://accidda.github.io/acciddasuite/reference/to_respilens.md)
to export the forecast as JSON for upload to
[MyRespiLens](https://www.respilens.com/myrespilens).

``` r

to_respilens(fcast, "respilens.json")
```
