# EpiEstim model for fable

Estimates the current reproduction number (Rt) using
[EpiEstim](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html) and
simulates future incidence using
[projections](https://www.repidemicsconsortium.org/projections/reference/project.html).
Works with any regular aggregation period and integrates into the fable
workflow via
[model()](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
EPIESTIM(
  formula,
  mean_si,
  std_si,
  rt_window = 14L,
  n_sim = 100L,
  R_fix_within = TRUE
)
```

## Arguments

- formula:

  Response variable, e.g. `observation`. Exogenous regressors are not
  supported.

- mean_si:

  Mean serial interval in days.

- std_si:

  Standard deviation of the serial interval in days.

- rt_window:

  Sliding window width in days for Rt estimation (`dt_out` in EpiEstim).
  Controls how many recent days inform the current Rt. Smaller values
  track recent trends more closely; larger values give a smoother
  estimate. Defaults to 14 days (~2 weeks).

- n_sim:

  Number of simulation paths for the forecast distribution.

- R_fix_within:

  If `TRUE`, Rt is held constant within each simulated path (recommended
  for short horizons).

## Value

A model definition for use inside
[model()](https://fabletools.tidyverts.org/reference/model.html).

## Details

The aggregation period is detected automatically from the tsibble index.
Only the most recent data (the `rt_window` estimation period plus the
serial interval period) is passed to EpiEstim's expectation-maximisation
(EM) algorithm, which reconstructs daily incidence from the aggregated
counts before estimating Rt on rolling `rt_window`-day windows. The most
recent Rt estimate is then used to project forward via stochastic
simulation. Forecasts are returned as sample distributions (one per
horizon period) drawn from `n_sim` epidemic paths.
