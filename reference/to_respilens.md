# Convert a forecast to RespiLens format

Convert a forecast to RespiLens format

## Usage

``` r
to_respilens(x, path = NULL)
```

## Arguments

- x:

  An `incast_fcast` from
  [`get_fcast`](https://accidda.github.io/incast/reference/get_fcast.md).

- path:

  Optional `.json` path to write to; if `NULL`, nothing is written.

## Value

A named list (`metadata`, `ground_truth`, `forecasts`), invisibly.
