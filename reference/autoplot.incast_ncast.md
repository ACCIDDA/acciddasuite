# Plot a nowcast

Plot reported counts alongside nowcast distributions. Nowcasts are shown
as the median, with 50% and 95% credible intervals.

## Usage

``` r
# S3 method for class 'incast_ncast'
autoplot(object, ...)
```

## Arguments

- object:

  An `incast_ncast` object returned by
  [`get_ncast`](https://accidda.github.io/incast/reference/get_ncast.md).

- ...:

  Ignored.

## Value

A ggplot object.

## Details

Each panel corresponds to a single time series.
