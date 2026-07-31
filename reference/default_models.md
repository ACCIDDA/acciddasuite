# Default forecasting models

Return the default set of forecasting models used by
[`get_cv`](https://accidda.github.io/incast/reference/get_cv.md) and
[`get_fcast`](https://accidda.github.io/incast/reference/get_fcast.md).

## Usage

``` r
default_models()
```

## Value

A named list of `fable` model specifications.

## Details

The default models are naive, ETS, Theta, and ARIMA models. All models
are fitted to `log(observation + 1)` to stabilise variance in count
data. Forecasts are automatically transformed back to the original scale
by `fable`. +1 is added to avoid taking the log of zero.

Additional models can be added by extending the returned list, for
example:
`c(default_models(), list(CUSTOM = fable::ARIMA(observation)))`.

## Author

Cyril Geismar

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
