# acciddasuite

`acciddasuite` provides a complete pipeline for infectious diseases
forecasts. It fetches
([`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md))
and validates input data
([`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md)),
optionally applies nowcasting to adjust for reporting delays
([`get_ncast()`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)),
evaluates models by cross-validation
([`get_cv()`](https://accidda.github.io/acciddasuite/reference/get_cv.md)),
and generates forecasts
([`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)).

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
#>   as_of      location target            target_end_date observation
#>   <date>     <chr>    <chr>             <date>                <dbl>
#> 1 2026-07-05 CA       wk inc covid hosp 2026-07-04              152
#> 2 2026-07-12 CA       wk inc covid hosp 2026-07-04              161
#> 3 2026-07-05 NY       wk inc covid hosp 2026-07-04               59
#> 4 2026-07-12 NY       wk inc covid hosp 2026-07-04               60
#> 5 2026-07-12 CA       wk inc covid hosp 2026-07-11              168
#> 6 2026-07-12 NY       wk inc covid hosp 2026-07-11               51
```

``` r

fcast <- example_data |>
  check_data() |>
  get_ncast() |>
  get_cv(eval_start_date = max(example_data$target_end_date) - 30) |>
  get_fcast(top_n = 3)
#> ℹ Using max_delay = 6 from data
#> ℹ Truncating from max_delay = 6 to 2.
#> ℹ Using max_delay = 6 from data
#> ℹ Truncating from max_delay = 6 to 2.
#> [2026-07-27 12:27:44.342] get_cv: +1.0240 secs
#> [2026-07-27 12:27:45.380] get_fcast: +5.0982 secs

fcast
#> <accidda_fcast>
#> Target:   wk inc covid hosp
#> Series:   2 (location)
#> Forecast: 2026-07-18 to 2026-08-08 (h = 4)
#> Models:   3 + ENSEMBLE

fcast |> autoplot()
```

![](reference/figures/README-unnamed-chunk-4-1.png)

Save to [myRespiLens](https://www.respilens.com/myrespilens) format:

``` r

# to_respilens(fcast, path = "respilens.json")
```
