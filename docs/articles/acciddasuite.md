# acciddasuite

## Introduction

The `acciddasuite` package provides tools for building infectious
disease forecasts and relies on the
[`fable`](https://fable.tidyverts.org/) framework.

This vignette demonstrates a basic example of generating and evaluating
forecasts following the standard forecasting workflow described by
[Hyndman & Athanasopoulos
(2021)](https://otexts.com/fpp3/basic-steps.html).

## Forecasting Workflow

### `get_data`

**If you would like to load your own surveillance, you can follow
[these](https://accidda.github.io/acciddasuite/articles/external_data.md)
steps for formatting.**

For demonstration purposes, we will load surveillance data from the [CDC
National Health Safety
Network](https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/mpgq-jmmr/about_data).
The
[`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md)
function provides a convenient interface to access this data using the
[`epidatr`](https://cmu-delphi.github.io/epidatr/) package.

``` r
library(dplyr)
library(ggplot2)
library(pipetime)
library(acciddasuite)
df <- get_data(pathogen = "covid", geo_values = "ny")
head(df)
#> # A tibble: 6 × 5
#>   as_of      location target            target_end_date observation
#>   <date>     <chr>    <chr>             <date>                <dbl>
#> 1 2026-03-08 NY       wk inc covid hosp 2020-08-08              517
#> 2 2026-03-08 NY       wk inc covid hosp 2020-08-15              490
#> 3 2026-03-08 NY       wk inc covid hosp 2020-08-22              844
#> 4 2026-03-08 NY       wk inc covid hosp 2020-08-29              483
#> 5 2026-03-08 NY       wk inc covid hosp 2020-09-05              479
#> 6 2026-03-08 NY       wk inc covid hosp 2020-09-12              573
```

To look at what `df` looks like, you can access the example `csv` file
here:
[example_data.csv](https://github.com/ACCIDDA/acciddasuite/blob/main/example_data.csv).

### Time Series Cross-Validation

To evaluate model performance, we employ *time series cross-validation*.
We fit models using the data available up to a specific cutoff point
(`eval_start_date`), then forecast `h` weeks ahead with expanding
windows. The further back in time `eval_start_date` is, the more
computationally intensive the evaluation step will be.

``` r
# We ony evaluate on the last 30 days of data for demonstration purposes
eval_start_date <- max(df$target_end_date) - 30
```

Default models are:  \* `SNAIVE` (Seasonal Naïve): Assumes this week
will look like the same week last year. The simplest possible baseline.
\* `ETS` (Exponential Smoothing): A weighted average where recent weeks
matter more than older ones. Adapts to trends and seasonal patterns. \*
`THETA`: Splits the data into a long-term trend and short-term
fluctuations, forecasts each separately, then combines them. \* `ARIMA`:
Learns repeating patterns from past values to predict future ones.
Auto-configured to find the best fit.

``` r
fcast = get_fcast(
  df,
  eval_start_date = eval_start_date,
  top_n = 4, # Select top 4 models
  h = 4 # forecast 4 weeks ahead
) |>
  time_pipe("base fcast", log = "timing")

fcast
#> <accidda_cast>
#> 
#> Models evaluated:
#>  model_id       wis
#>    <char>     <num>
#>     THETA  26.88197
#>     ARIMA  29.14217
#>       ETS  31.33395
#>  ENSEMBLE  52.64632
#>    SNAIVE 256.25401
#> 
#> Forecast horizon:
#>   From: 2026-02-07 
#>   To:   2026-04-04 
#> 
#> Contents:
#>   $hubcast   hub forecast object
#>   $score     model ranking table
#>   $plot      ggplot2 object
```

Visualize forecasts by accessing the `plot` element of the forecast
object:

``` r
fcast$plot
```

![](acciddasuite_files/figure-html/plot-forecast-1.png)

### Adding `extra_models`

Additonal models can be added by defining them in a list and passing
them to
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).
The models should be compatible with the fable framework (see [fable
documentation](https://fabletools.tidyverts.org/articles/extension_models.html)
for more information).

``` r
library(fable)
library(fable.prophet)
extra <- list(
  CUSTOM_ARIMA = ARIMA(observation ~ pdq(1,1,0)),
  PROPHET = prophet(observation ~ season("year")),
  EPIESTIM = EPIESTIM(observation, mean_si = 3, std_si = 2, rt_window = 7)
)

fcast = get_fcast(
  df,
  eval_start_date = eval_start_date,
  top_n = 4, # Select top 4 models
  h = 3, # forecast 3 weeks ahead,
  extra_models = extra
) |>
  time_pipe("extra fcast", log = "timing")
```

You can check how long each step took by calling
[`pipetime::get_log()`](https://rdrr.io/pkg/pipetime/man/get_log.html):

``` r
get_log()
#> $timing
#>             timestamp       label duration unit
#> 1 2026-03-18 14:24:26  base fcast 2.417592 secs
#> 2 2026-03-18 14:24:29 extra fcast 2.867576 secs
```

## Submit to MyRespiLens

[RespiLens](https://www.respilens.com/) is a platform for sharing and
visualizing respiratory disease forecasts. To submit forecasts to
RespiLens, you can use
[`to_respilens()`](https://accidda.github.io/acciddasuite/reference/to_respilens.md)
to save the file in JSON format and upload it to MyRespiLens
[here](https://www.respilens.com/myrespilens).

``` r
to_respilens(fcast, "respilens.json")
```
