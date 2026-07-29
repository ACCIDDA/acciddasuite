# Plot a forecast

Plot observed values alongside forecast distributions for a selected
model. Forecasts are shown as the median, with 50% and 95% prediction
intervals. By default, the ensemble forecast is displayed.

## Usage

``` r
# S3 method for class 'accidda_fcast'
autoplot(object, model = "ENSEMBLE", ...)
```

## Arguments

- object:

  An `accidda_fcast` object returned by
  [`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

- model:

  The model to plot. Defaults to `"ENSEMBLE"`.

- ...:

  Ignored.

## Value

A ggplot object.

## Details

Each panel corresponds to a single time series.
