
<!-- README.md is generated from README.Rmd. Please edit that file -->

# acciddasuite <a href="https://accidda.github.io/acciddasuite/"><img src="man/figures/logo.png" align="right" height="139" alt="acciddasuite website" /></a>

<!-- badges: start -->

<!-- badges: end -->

The goal of acciddasuite is to …

## Installation

You can install the development version of acciddasuite from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
#pak::pak("ACCIDDA/acciddasuite")
```

## Example

``` r
library(acciddasuite)
df <- get_data(pathogen = "flu", geo_values = "ny")
fcast <- df |> 
  get_fcast(eval_start_date = max(df$target_end_date) - 30)
fcast
#> <accidda_cast>
#> 
#> Models evaluated:
#>  model_id        wis
#>    <char>      <num>
#>     ARIMA   85.65125
#>       ETS  120.98738
#>  ENSEMBLE  128.35163
#>     THETA  214.99363
#>    SNAIVE 1351.92228
#> 
#> Forecast horizon:
#>   From: 2026-01-31 
#>   To:   2026-03-28 
#> 
#> Contents:
#>   $hubcast   hub forecast object
#>   $score     model ranking table
#>   $plot      ggplot2 object
```

``` r
fcast$plot
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />

Save to [myRespiLens](https://www.respilens.com/myrespilens) format:

``` r
to_respilens(fcast, path = "example_respilens.json")
```
