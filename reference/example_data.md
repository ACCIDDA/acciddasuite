# Weekly COVID-19 hospital admissions for New York

Weekly confirmed COVID-19 hospital admissions for New York (CDC NHSN),
with revision history, fetched via
[`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md);
pass through
[`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
to use it. Covers Aug 2020 to Mar 2026.

## Usage

``` r
example_data
```

## Format

A data frame with 5 columns:

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

## Examples

``` r
if (FALSE) { # \dontrun{
example_data |> check_data()
} # }
```
