# Validate surveillance data

Validate and standardise surveillance data for use throughout the
package. The returned `incast_data` object can be passed directly to
forecasting and nowcasting functions.

## Usage

``` r
check_data(data, key = "location")
```

## Arguments

- data:

  A data frame containing `target_end_date` (`Date`), `observation`
  (numeric), `target` (character), and one or more key columns. An
  optional `as_of` (`Date`) column enables nowcasting with
  [`get_ncast`](https://accidda.github.io/incast/reference/get_ncast.md).
  If `data` is already an `incast_data` object, it is returned
  unchanged.

- key:

  Character vector giving the column name(s) that uniquely identify each
  time series, equivalent to the key of a
  [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html).
  Each unique combination of key values is treated as a separate series.
  Defaults to `"location"`.

## Value

An `incast_data` object containing:

- data:

  Validated data with standardised column types.

- key:

  Names of the key columns.

- target:

  Target variable name.

- window:

  Start and end dates of the data.

- interval:

  Reporting interval in days (for example, 7 for weekly data).

- history:

  Logical indicating whether revision history (`as_of`) is available.

## Details

Data must contain one row per time series and reporting date (and
`as_of`, if present). All series must have the same reporting interval,
share the same reporting dates, and end on the same date. Series may
begin at different times and may contain missing reporting periods.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- get_data("covid", c("ny", "ca")) |> check_data()
my_x <- read.csv("my_data.csv") |>
  check_data(key = c("location", "age_group"))
} # }
```
