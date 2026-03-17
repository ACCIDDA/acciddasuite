# Get hospitalisation data

Fetch the most recent confirmed US hospital admissions from the NHSN
source for COVID-19, influenza, or RSV using
[pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html).

## Usage

``` r
get_data(pathogen, geo_values = "*")
```

## Arguments

- pathogen:

  Character. One of "covid", "flu" or "rsv".

- geo_values:

  Character vector. Geographic values as per
  [pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html)
  to filter by. Defaults to "\*" (all states).

## Value

A tibble in `hubverse` format with columns:

- `as_of`: Date when the data was issued

- `location`: State code

- `target`: Target description (e.g., "wk inc covid hosp")

- `target_end_date`: End date of the target week

- `observation`: Observed hospital admissions

## See also

[pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html)

https://docs.hubverse.io/en/stable/user-guide/target-data.html

## Examples

``` r
get_data(pathogen = "covid", geo_values = c("ca", "ny"))
#> Warning: No API key found. You will be limited to non-complex queries and encounter rate
#> limits if you proceed.
#> ℹ See `?save_api_key()` for details on obtaining and setting API keys.
#> This warning is displayed once every 8 hours.
#> # A tibble: 584 × 5
#>    as_of      location target            target_end_date observation
#>    <date>     <chr>    <chr>             <date>                <dbl>
#>  1 2026-03-08 CA       wk inc covid hosp 2020-08-08             4836
#>  2 2026-03-08 NY       wk inc covid hosp 2020-08-08              517
#>  3 2026-03-08 CA       wk inc covid hosp 2020-08-15             4273
#>  4 2026-03-08 NY       wk inc covid hosp 2020-08-15              490
#>  5 2026-03-08 CA       wk inc covid hosp 2020-08-22             3498
#>  6 2026-03-08 NY       wk inc covid hosp 2020-08-22              844
#>  7 2026-03-08 CA       wk inc covid hosp 2020-08-29             3100
#>  8 2026-03-08 NY       wk inc covid hosp 2020-08-29              483
#>  9 2026-03-08 CA       wk inc covid hosp 2020-09-05             2888
#> 10 2026-03-08 NY       wk inc covid hosp 2020-09-05              479
#> # ℹ 574 more rows
```
