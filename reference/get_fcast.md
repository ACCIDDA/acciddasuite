# Produce a forward-looking forecast

Refits models on the full series, forecasts `h` steps ahead and combines
them into an equal-weight ensemble. Accepts either an `accidda_cv` from
[`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md) —
whose ranking selects the best `top_n` models — or an `accidda_data` /
`accidda_ncast`, which forecasts every model in `models`.

## Usage

``` r
get_fcast(x, models = default_models(), h = 4, top_n = 3)
```

## Arguments

- x:

  An `accidda_cv`
  ([`get_cv`](https://accidda.github.io/acciddasuite/reference/get_cv.md)),
  `accidda_ncast` or `accidda_data`.

- models:

  Named list of `fable` models. Defaults to
  [`default_models`](https://accidda.github.io/acciddasuite/reference/default_models.md).
  For an `accidda_cv`, leave unset to forecast its `top_n` ranked
  models, or pass `models` to forecast a set of your own.

- h:

  Integer. Forecast horizon, in reporting-interval steps (weeks for
  weekly data). Default 4; for an `accidda_cv`, defaults to the
  cross-validation horizon.

- top_n:

  Integer. Number of top-ranked CV models to ensemble. Used only when
  `x` is an `accidda_cv` and `models` is unset. Default 3.

## Value

An `accidda_fcast` object:

- hub:

  Hub-format forecast (`model_out_tbl`, `oracle_output`).

- score:

  Model ranking from the `accidda_cv`, or `NULL`.

- meta:

  `models`, `top_n`, `h`, `location`, `target`, `interval`, `nowcast`.

Export with
[`to_respilens`](https://accidda.github.io/acciddasuite/reference/to_respilens.md).

## Details

If the input carries `ncast_lower` / `ncast_upper` (from
[`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)),
forecasts are pooled across the nowcast median and 95\\

## Examples

``` r
if (FALSE) { # \dontrun{
ncast <- get_data("covid", "ny", revisions = TRUE) |> get_ncast()
cv    <- ncast |> get_cv(eval_start_date = "2025-01-01", h = 4)

get_fcast(cv, top_n = 3)                 # reuse the cross-validation ranking
get_fcast(cv, models = default_models()) # forecast a different set; keeps $score
get_fcast(ncast)                         # or forecast the default models directly
} # }
```
