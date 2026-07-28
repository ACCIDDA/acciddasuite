
<!-- README.md is generated from README.Rmd. Please edit that file -->

# acciddasuite <a href="https://accidda.github.io/acciddasuite/"><img src="man/figures/logo.png" align="right" height="139" alt="acciddasuite website" /></a>

<!-- badges: start -->

<!-- badges: end -->

`acciddasuite` provides a complete pipeline for infectious diseases
forecasts. It fetches (`get_data()`) and validates input data
(`check_data()`), optionally applies nowcasting to adjust for reporting
delays (`get_ncast()`), evaluates models by cross-validation
(`get_cv()`), and generates forecasts (`get_fcast()`).

## Installation

You can install the development version of acciddasuite from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
# pak::pak("ACCIDDA/acciddasuite")
```

## Example

``` r
library(acciddasuite)
tail(example_data)
#> # A tibble: 6 × 5
#>   as_of      location target          target_end_date observation
#>   <date>     <chr>    <chr>           <date>                <dbl>
#> 1 2025-12-07 CA       wk inc flu hosp 2025-12-06              233
#> 2 2025-12-14 CA       wk inc flu hosp 2025-12-06              259
#> 3 2025-12-07 NY       wk inc flu hosp 2025-12-06             1160
#> 4 2025-12-14 NY       wk inc flu hosp 2025-12-06             1171
#> 5 2025-12-14 CA       wk inc flu hosp 2025-12-13              412
#> 6 2025-12-14 NY       wk inc flu hosp 2025-12-13             1462
```

``` r
fcast <- example_data |>
  check_data() |>
  get_ncast() |>
  get_cv(eval_start_date = max(example_data$target_end_date) - 30) |>
  get_fcast()
#> ℹ Using max_delay = 6 from data
#> ℹ Truncating from max_delay = 6 to 2.
#> ℹ Using max_delay = 6 from data
#> ℹ Truncating from max_delay = 6 to 2.
#> [2026-07-28 13:58:52.143] get_cv: +0.9873 secs
#> [2026-07-28 13:58:53.142] get_fcast: +5.0517 secs

fcast
#> <accidda_fcast>
#> Target:   wk inc flu hosp
#> Series:   2 (location)
#> Forecast: 2025-12-20 to 2026-01-10 (h = 4)
#> Models:   4 + ENSEMBLE

fcast |> autoplot()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" alt="" width="100%" />

Save to [myRespiLens](https://www.respilens.com/myrespilens) format:

``` r
to_respilens(fcast, path = "respilens.json")
```
