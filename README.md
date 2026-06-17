
<!-- README.md is generated from README.Rmd. Please edit that file -->

# acciddasuite <a href="https://accidda.github.io/acciddasuite/"><img src="man/figures/logo.png" align="right" height="139" alt="acciddasuite website" /></a>

<!-- badges: start -->

<!-- badges: end -->

`acciddasuite` provides a simple pipeline for infectious diseases
forecasts. It validates input data (`check_data()`), optionally applies
nowcasting to adjust for reporting delays (`get_ncast()`), evaluates
models by cross-validation (`get_cv()`), and generates forecasts
(`get_fcast()`).

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
head(example_data)
#> # A tibble: 6 × 5
#>   as_of      location target            target_end_date observation
#>   <date>     <chr>    <chr>             <date>                <dbl>
#> 1 2024-11-17 NY       wk inc covid hosp 2020-08-08              517
#> 2 2024-11-24 NY       wk inc covid hosp 2020-08-08              517
#> 3 2024-12-01 NY       wk inc covid hosp 2020-08-08              517
#> 4 2024-12-08 NY       wk inc covid hosp 2020-08-08              517
#> 5 2024-12-15 NY       wk inc covid hosp 2020-08-08              517
#> 6 2024-12-22 NY       wk inc covid hosp 2020-08-08              517
```

``` r
fcast <- example_data |>
  check_data() |>
  get_ncast() |>
  get_cv(eval_start_date = max(example_data$target_end_date) - 28) |>
  get_fcast(top_n = 3)
#> ℹ Using max_delay = 6 from data
#> ℹ Truncating from max_delay = 6 to 2.
#> [2026-06-17 19:07:08.646] get_cv: +0.7772 secs
#> [2026-06-17 19:07:09.427] get_fcast: +2.2583 secs

fcast
#> <accidda_fcast>
#> 
#> Models ranked (cross-validation):
#> # A tibble: 4 × 2
#>   model_id   wis
#>   <chr>    <dbl>
#> 1 THETA     26.3
#> 2 ARIMA     28.6
#> 3 ETS       30.4
#> 4 SNAIVE   256. 
#> 
#> Forecast horizon:
#>   From: 2026-03-21 
#>   To:   2026-04-11 
#> 
#> Contents:
#>   $hub    hub forecast object (model_out_tbl, oracle_output)
#>   $score  model ranking table, or NULL
#>   $meta   models, top_n, h, location, target, interval, nowcast
```

Save to [myRespiLens](https://www.respilens.com/myrespilens) format:

``` r
to_respilens(fcast, path = "respilens.json")
```
