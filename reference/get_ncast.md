# Nowcast right-truncated surveillance data

Recent weeks of surveillance data are incomplete because of reporting
delays (right truncation). `get_ncast` estimates their final counts with
[baselinenowcast](https://baselinenowcast.epinowcast.org/), replacing
the last `max_delay` weeks and leaving earlier weeks untouched.

## Usage

``` r
get_ncast(x, max_delay = 2, draws = 1000, prop_delay = 0.5, scale_factor = 3)
```

## Arguments

- x:

  An `accidda_data`
  ([`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md)
  /
  [`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md))
  with revision history; use `get_data(revisions = TRUE)`.

- max_delay:

  Integer. Number of recent weeks treated as right-truncated. Default 2.

- draws:

  Integer. Number of posterior samples. Default 1000.

- prop_delay:

  Numeric in (0, 1). Proportion of reference times used for delay
  estimation. Default 0.5.

- scale_factor:

  Numeric. Multiplier on `max_delay` for the estimation window. Default
  3.

## Value

An `accidda_ncast` object with the shared backbone (`location`,
`target`, `window`, `interval`, `history`) plus:

- data:

  Corrected series. `observation` holds the nowcast median for corrected
  weeks; `ncast_lower` / `ncast_upper` (95\\ propagate nowcast
  uncertainty.

- plot:

  ggplot of the correction.

## Details

Weekly data only; other cadences are rejected (the rest of the pipeline
is cadence-agnostic).

## Examples

``` r
if (FALSE) { # \dontrun{
x     <- get_data(pathogen = "covid", geo_value = "ca", revisions = TRUE)
ncast <- get_ncast(x)
} # }
```
