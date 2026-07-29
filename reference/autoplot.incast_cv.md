# Plot cross-validation model rankings

Plot model performance across time series using the weighted interval
score (WIS). By default, models are ranked using relative WIS, where
values below 1 indicate better-than-average performance and values above
1 indicate worse-than-average performance. If only one model is present,
raw WIS values are displayed instead.

## Usage

``` r
# S3 method for class 'incast_cv'
autoplot(object, ...)
```

## Arguments

- object:

  An `incast_cv` object returned by
  [`get_cv`](https://accidda.github.io/incast/reference/get_cv.md).

- ...:

  Ignored.

## Value

A ggplot object.

## Details

Each panel corresponds to a single time series.
