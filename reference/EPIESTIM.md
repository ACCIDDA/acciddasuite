# EpiEstim model for fable

A `fable` model that estimates the reproduction number Rt with
[`EpiEstim::estimate_R()`](https://rdrr.io/pkg/EpiEstim/man/estimate_R.html)
and projects future incidence with
[`projections::project()`](https://www.repidemicsconsortium.org/projections/reference/project.html),
for use inside
[model](https://fabletools.tidyverts.org/reference/model.html). The
aggregation period is read from the tsibble index; Rt is estimated on
the most recent data and used to simulate `n_sim` forward paths,
returned as sample distributions.

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

- mean_si, std_si:

  Mean and SD of the serial interval, in days.

- rt_window:

  Sliding window width (days) for Rt estimation. Smaller tracks recent
  trends; larger is smoother. Default 14.

- n_sim:

  Number of simulated forecast paths. Default 100.

- R_fix_within:

  If `TRUE`, hold Rt constant within each simulated path (recommended
  for short horizons).

## Value

A model definition for use inside
[model](https://fabletools.tidyverts.org/reference/model.html).
