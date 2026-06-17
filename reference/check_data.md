# Validate surveillance data

Validates a surveillance data frame and returns a typed `accidda_data`
object that the rest of the pipeline accepts without re-validating.

## Usage

``` r
check_data(data)
```

## Arguments

- data:

  A data frame with `target_end_date` (Date), `observation` (numeric),
  `location` (character) and `target` (character). An optional `as_of`
  (Date) column enables nowcasting
  ([`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)).
  An existing `accidda_data` is returned unchanged.

## Value

An `accidda_data` object:

- data:

  Validated data frame with coerced types.

- location, target:

  Single location / target identifier.

- window:

  Named `from` / `to` dates.

- interval:

  Reporting interval in days (7 = weekly).

- history:

  `TRUE` if revision history (`as_of`) is present.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- get_data("covid", "ny") |> check_data()
my_x <- read.csv("my_data.csv") |> check_data()
} # }
```
