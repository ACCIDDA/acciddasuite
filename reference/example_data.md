# Weekly COVID-19 hospital admissions for New York

Weekly confirmed COVID-19 hospital admissions for New York state from
the CDC NHSN, fetched via
[`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md)
with revision history. Covers August 2020 through March 2026.

## Usage

``` r
example_data
```

## Format

A data frame with 292 rows and 5 columns:

- as_of:

  Date the observation was reported.

- location:

  State abbreviation (`"NY"`).

- target:

  Forecast target (`"wk inc covid hosp"`).

- target_end_date:

  End date of the epiweek.

- observation:

  Confirmed hospital admissions count.

## Source

CDC NHSN via
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html).

## Details

Pass this data frame through
[`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
to use it in the acciddasuite pipeline.

## Examples

``` r
if (FALSE) { # \dontrun{
example_data |> check_data() |> get_fcast(eval_start_date = "2025-01-01")
} # }
```
