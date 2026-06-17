# Cross-validate forecasting models

Evaluates `fable` models by expanding-window time-series
cross-validation: from `eval_start_date`, models are refit at each
origin and forecast `h` steps ahead, then scored by weighted interval
score (WIS) and interval coverage via `hubEvals`. The ranking is reused
by
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

## Usage

``` r
get_cv(x, eval_start_date, h = 4, models = default_models())
```

## Arguments

- x:

  An `accidda_ncast`
  ([`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md))
  or `accidda_data`
  ([`check_data`](https://accidda.github.io/acciddasuite/reference/check_data.md)
  /
  [`get_data`](https://accidda.github.io/acciddasuite/reference/get_data.md)).

- eval_start_date:

  Date (or string coercible to one). First origin evaluated; must lie
  within the data window with at least one year of data before it.

- h:

  Integer. Forecast horizon (e.g. 1 week for weekly data). Default 4.

- models:

  Named list of `fable` models. Defaults to
  [`default_models`](https://accidda.github.io/acciddasuite/reference/default_models.md);
  compose with `c(default_models(), list(...))` to extend. Each element
  must reference `observation`. See the [fabletools extension
  vignette](https://fabletools.tidyverts.org/articles/extension_models.html)
  for custom models.

## Value

An `accidda_cv` object:

- forecasts:

  Per-origin, per-model forecasts (`model_out_tbl`).

- oracle:

  Observed truth (`oracle_output`).

- score:

  Model ranking by WIS with interval coverage.

- models:

  The evaluated model specifications.

- meta:

  `eval_start_date`, `h`, `location`, `target`, `interval`.

- data:

  Revision-collapsed input, reused by
  [`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cv <- get_data("covid", "ny", revisions = TRUE) |>
  get_ncast() |>
  get_cv(eval_start_date = "2025-01-01", h = 4)

cv$score
} # }
```
