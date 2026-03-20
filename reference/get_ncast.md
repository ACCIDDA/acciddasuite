# Nowcast right-truncated surveillance data

The most recent weeks of surveillance data are almost always incomplete
because of reporting delays (right truncation). `get_ncast` uses
[baselinenowcast](https://baselinenowcast.epinowcast.org/) to estimate
what those counts will look like once all reports arrive.

## Usage

``` r
get_ncast(df, max_delay = 4, draws = 1000, prop_delay = 0.5, scale_factor = 3)
```

## Arguments

- df:

  An `accidda_data` object from
  [`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
  or
  [`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md).
  Must have revision history (`$history == TRUE`); use
  `get_data(revisions = TRUE)`.

- max_delay:

  Integer. Maximum reporting delay in weeks. Default 4.

- draws:

  Integer. Number of posterior samples. Default 1000.

- prop_delay:

  Numeric 0-1. Proportion of reference times used for delay estimation.
  Default 0.5.

- scale_factor:

  Numeric. Multiplicative factor on the maximum delay for the estimation
  window. Default 3.

## Value

An `accidda_ncast` object (a list) with:

- data:

  Corrected time series. The `observation` column contains the nowcast
  median for the corrected weeks. Two extra columns, `ncast_lower` and
  `ncast_upper` (95\\ used by
  [`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
  to propagate nowcast uncertainty.

- plot:

  ggplot2 visualisation of the nowcast correction.

## Details

With the default `max_delay = 4`, the last 4 weeks are treated as
right-truncated and replaced by nowcast estimates. Everything before
that is left untouched.

The function returns three corrected versions of the full series
(nowcast median, lower, and upper 95\\
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
can propagate nowcast uncertainty into the final forward-looking
forecast.

## Examples

``` r
if (FALSE) { # \dontrun{
df    <- get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
ncast <- get_ncast(df)
fcast <- get_fcast(ncast, eval_start_date = "2025-01-01")
} # }
```
