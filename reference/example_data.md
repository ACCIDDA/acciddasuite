# Weekly influenza hospital admissions for New York and California

Weekly confirmed influenza hospital admissions for New York and
California (CDC NHSN), with revision history, fetched via
[`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md);
pass through
[`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
to use it.

## Usage

``` r
example_data
```

## Format

A data frame with 5 columns:

- as_of:

  Date the observation was reported.

- location:

  State abbreviation (`"NY"` or `"CA"`).

- target:

  Forecast target (`"wk inc flu hosp"`).

- target_end_date:

  End date of the epiweek.

- observation:

  Confirmed hospital admissions count.

## Source

CDC NHSN via
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html).

## Details

The archive is pinned to the 14 December 2025 history, so the most
recent weeks are still right-truncated and
[`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)
has reporting delay to model. To regenerate with
`data-raw/example_data.R`.

## Examples

``` r
if (FALSE) { # \dontrun{
example_data |> check_data()
} # }
```
