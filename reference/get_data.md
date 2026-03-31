# Get hospitalisation data

Fetch confirmed US hospital admissions from the NHSN source for
COVID-19, influenza, or RSV using
[pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html).

## Usage

``` r
get_data(pathogen, geo_value, revisions = FALSE)
```

## Arguments

- pathogen:

  Character. One of "covid", "flu" or "rsv".

- geo_value:

  Character vector. Geographic value as per
  [pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html)
  to filter by.

- revisions:

  Logical. If `TRUE`, fetches all available revision history, producing
  data ready for
  [`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md).
  Default is `FALSE` (latest version only).

## Value

An `accidda_data` object (see
[`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)).

## Details

By default the latest version of each observation is returned. Set
`revisions = TRUE` to retrieve all available revision history, which is
needed by
[`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md).

## See also

[pub_covidcast](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html)

https://docs.hubverse.io/en/stable/user-guide/target-data.html

## Examples

``` r
get_data(pathogen = "covid", geo_value = "ny")
#> Warning: No API key found. You will be limited to non-complex queries and encounter rate
#> limits if you proceed.
#> ℹ See `?save_api_key()` for details on obtaining and setting API keys.
#> This warning is displayed once every 8 hours.
#> <accidda_data>
#> 
#> Location: NY 
#> Target:   wk inc covid hosp 
#> Window:   2020-08-08 to 2026-03-21 ( 294 dates )
#> History:  FALSE

# Fetch revision history for nowcasting
get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
#> <accidda_data>
#> 
#> Location: CA 
#> Target:   wk inc covid hosp 
#> Window:   2020-08-08 to 2026-03-21 ( 294 dates )
#> History:  TRUE ( 2024-11-17 to 2026-03-22 )
```
