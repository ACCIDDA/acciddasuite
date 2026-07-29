# Plot cross-validation model rankings

Plot model performance across time series using the weighted interval
score (WIS). By default, models are ranked using relative WIS, where
values below 1 indicate better-than-average performance and values above
1 indicate worse-than-average performance.

## Usage

``` r
# S3 method for class 'accidda_cv'
autoplot(object, ...)
```

## Arguments

- object:

  An `accidda_cv` object returned by
  [`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md).

- ...:

  Ignored.

## Value

A ggplot object.

## Details

Each panel corresponds to a single time series and uses a common log
scale and model ordering to allow comparisons across panels. If only one
model is present, raw WIS values are displayed instead.

For custom metrics or visualisations, use `object$score` directly.
