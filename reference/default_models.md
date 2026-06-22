# Default forecasting models

The built-in model set for
[`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md)
and
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md):
a naive random walk, ETS, Theta and ARIMA, each fitted on
`log(observation)` to stabilise count variance (`fable` back-transforms
automatically). Extend rather than replace by composing, e.g.
`c(default_models(), list(CUSTOM = fable::ARIMA(observation)))`.

## Usage

``` r
default_models()
```

## Value

A named list of fable model definitions.

## Examples

``` r
default_models()
#> $NAIVE
#> <RW model definition>
#> 
#> $ETS
#> <ETS model definition>
#> 
#> $THETA
#> <theta model definition>
#> 
#> $ARIMA
#> <ARIMA model definition>
#> 
```
