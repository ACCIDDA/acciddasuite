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
#> 
#> Location: NY 
#> Target:   wk inc covid hosp 
#> Window:   2020-08-08 to 2026-06-06 ( 305 dates )
#> Interval: 7 days
#> History:  TRUE ( 2024-11-17 to 2026-06-07 )
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
With the default `max_delay = 4`, the last 4 weeks are corrected;
everything before that is left untouched.

``` r

ncast <- get_ncast(df)
ncast
#> <accidda_ncast>
#> 
#> Location: NY 
#> Target:   wk inc covid hosp 
#> Nowcasted 2 weeks: 2026-05-30 to 2026-06-06 
#> 
#> $data  corrected series (305 rows)
#> $plot  nowcast visualisation
```

``` r

ncast$plot
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
    — model selection**: time series cross-validation on the full
    (median corrected) series, starting from `eval_start_date`. Models
    are ranked by WIS and interval coverage.
2.  **[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
    — final forecast**: reuses the ranking to ensemble the best `top_n`
    models and projects `h` weeks into the future. When nowcast columns
    are present, the forecast is produced from three baselines (lower,
    median, and upper nowcast estimates) and pooled, so prediction
    intervals reflect both model uncertainty and nowcast uncertainty.

We set `eval_start_date` to mark the start of the evaluation window. At
least 52 weeks of data must precede this date.

``` r

eval_start_date <- max(ncast$data$target_end_date) - 28
```

Default models are:

- `SNAIVE` (Seasonal Naïve): Assumes this week will look like the same
  week last year. The simplest possible baseline.

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
#> 
#> Models ranked (cross-validation):
#>  model_id       wis
#>    <char>     <num>
#>       ETS  29.43926
#>     ARIMA  31.71773
#>     THETA  34.14236
#>    SNAIVE 160.26634
#> 
#> Evaluated from 2026-05-09 | horizon 4 weeks | NY 
#> 
#> Contents:
#>   $forecasts  per-origin forecasts (model_out_tbl)
#>   $oracle     observed truth (oracle_output)
#>   $score      model ranking table
#>   $models     model specifications
#>   $meta       eval_start_date, h, location, target, interval
```

``` r

fcast <- get_fcast(cv, top_n = 3)
fcast
#> <accidda_fcast>
#> 
#> Models ranked (cross-validation):
#>  model_id       wis
#>    <char>     <num>
#>       ETS  29.43926
#>     ARIMA  31.71773
#>     THETA  34.14236
#>    SNAIVE 160.26634
#> 
#> Forecast horizon:
#>   From: 2026-06-13 
#>   To:   2026-07-04 
#> 
#> Contents:
#>   $hub    hub forecast object (model_out_tbl, oracle_output)
#>   $score  model ranking table, or NULL
#>   $meta   models, top_n, h, location, target, interval, nowcast
```

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
    CUSTOM_ARIMA = ARIMA(observation ~ pdq(1,1,0)),
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
