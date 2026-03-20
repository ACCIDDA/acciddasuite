# Preparing External Data

## Bringing your own data

You can use any surveillance dataset with `acciddasuite` — just pass it
through
[`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md)
to validate and enter the pipeline.

## Required columns

Your data frame must have these 4 columns:

| Column            | Type      | Description                                             |
|-------------------|-----------|---------------------------------------------------------|
| `target_end_date` | Date      | The date for which an observation is recorded           |
| `observation`     | numeric   | The observed value                                      |
| `location`        | character | A single location identifier                            |
| `target`          | character | A single target identifier (e.g., “inc hosp influenza”) |

To enable **nowcasting** (correcting for reporting delays), add a 5th
column:

| Column  | Type | Description                                   |
|---------|------|-----------------------------------------------|
| `as_of` | Date | The date the observation was reported/revised |

With `as_of`, the same `target_end_date` can appear multiple times (one
row per revision). Without it, each `target_end_date` should appear
once.

## Example

``` r
library(acciddasuite)
head(df)
```

    ##   target_end_date observation location             target
    ## 1      2024-01-01          13       NY inc hosp influenza
    ## 2      2024-01-08          15       NY inc hosp influenza
    ## 3      2024-01-15          19       NY inc hosp influenza
    ## 4      2024-01-22          22       NY inc hosp influenza
    ## 5      2024-01-29          25       NY inc hosp influenza
    ## 6      2024-02-05          11       NY inc hosp influenza

``` r
checked <- check_data(df)
checked
```

    ## <accidda_data>
    ## 
    ## Location: NY 
    ## Target:   inc hosp influenza 
    ## Window:   2024-01-01 to 2024-12-23 ( 52 dates )
    ## History:  FALSE

The
[`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md)
output tells you whether revision history is available. From here you
can go directly to forecasting:

``` r
get_fcast(checked, eval_start_date = "2024-11-01", h = 4)
```

Or, if your data has revision history, nowcast first:

``` r
checked |> get_ncast() |> get_fcast(eval_start_date = "2024-11-01", h = 4)
```
