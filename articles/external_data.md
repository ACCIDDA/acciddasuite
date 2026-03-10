# Preparing External Data

## Utilizing external ground truth data

The `acciddasuite` supports forecasting of hospitalization incidence due
to COVID-19, RSV, or influenza based on given ground truth data. While
you are able to quickly pull state-level `inc hosp` data using
[`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md),
you may desire to use your own hospitalization ground truth data for
forecasting (i.e., as the `df` parameter for modeling in
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)).
To do so, you must esnure your data adheres to the format described
below. Please note that
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
only processes one location at a time; so if you have multiple
locations’ ground truth data, you should separate your ground truth
dataset into one data frame per location and then initiate a
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md)
run for each.

### Columns and data types

Your ground truth data must contain 4 distinct columns:

| column name       | data type | description                                                                                                                                          |
|-------------------|-----------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| `target_end_date` | Date      | The date for which an observation is recorded                                                                                                        |
| `observation`     | numeric   | The observed value for a given date                                                                                                                  |
| `location`        | character | The location for which the data describes. You may use any location identifier you like, as long as it is consistent and represented as a character. |
| `target`          | character | The data stream being observed (e.g, “inc hosp influenza” or “inc hosp rsv”)                                                                         |

### Data content restrictions

Once your data has `target_end_date`, `observation`, `location` and
`target` columns, you must ensure the following for successful
processing in
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md):

- `target_end_date` values should be unique and have no NAs (i.e., there
  should be no duplicate or missing values in the `target_end_date`
  column)
- `target_end_date` values should be
- Your data must be on a **weekly** cadence (i.e., `target_end_date`
  values should be exactly one week apart, **and be continuous from week
  1 to end**)
- The `target` column must contain only one unique value (i.e, the same
  target is recorded for all of your data)
- Your dataset should contain ≥ 52 entries (weeks of observations; rows)
  where the `target_end_date` is \< the `eval_start_date` parameter you
  pass into
  [`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).
  Please note that NA entries in the `observation` column will be
  filtered out silently, and may result in the reduction of number of
  observations in your dataset. It is optimal to ensure that NA values
  are resolved prior to use in
  [`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

After ensuring compliance with all of the stipulations above, you can
read your dataset in (either as `data.frame` or `tibble`) and pass it as
the `df` parameter in
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

### Final form data

Properly converted data, for example, your `df` may resemble this:

    ##   target_end_date observation location             target
    ## 1      2024-01-01          13       NY inc hosp influenza
    ## 2      2024-01-08          15       NY inc hosp influenza
    ## 3      2024-01-15          19       NY inc hosp influenza
    ## 4      2024-01-22          22       NY inc hosp influenza
    ## 5      2024-01-29          25       NY inc hosp influenza
    ## 6      2024-02-05          11       NY inc hosp influenza

Data type compliance:

``` r
class(df$target_end_date)
```

    ## [1] "Date"

``` r
class(df$observation)
```

    ## [1] "integer"

``` r
class(df$location)
```

    ## [1] "character"

``` r
class(df$target)
```

    ## [1] "character"

Which can now be used as the `df` parameter in
[`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md):

``` r
x = get_fcast(
    df,
    eval_start_date = "2025-01-01",
    h = 3,
    top_n = 5
)
```

where `eval_start_date` (`2025-01-01`) begins after 52 consecutive weeks
of `target_end_date` entries:

``` r
sum(df$target_end_date < eval_start_date, na.rm = TRUE)
```

    ## [1] 52

You can have more than 52 entries preceding your `eval_start_date`, but
52 is the minimum.
