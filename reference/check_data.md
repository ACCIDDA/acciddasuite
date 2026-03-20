# Validate surveillance data for the acciddasuite pipeline

Checks that a data frame has the required columns and structure for use
with
[`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)
and
[`get_fcast`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).
Returns a typed `accidda_data` object that downstream functions can
accept without repeating validation.

## Usage

``` r
check_data(df)
```

## Arguments

- df:

  A data frame (or tibble) with at least: `target_end_date` (Date),
  `observation` (numeric), `location` (character), and `target`
  (character). An optional `as_of` (Date) column enables nowcasting via
  [`get_ncast`](https://accidda.github.io/acciddasuite/reference/get_ncast.md).

## Value

An `accidda_data` object (a list) with:

- data:

  The validated data frame with coerced date types.

- location:

  Single location identifier.

- target:

  Single target identifier.

- window:

  Named vector with `from` and `to` dates.

- history:

  Logical. `TRUE` if revision history (`as_of`) is present.

## Examples

``` r
if (FALSE) { # \dontrun{
# From get_data
df <- get_data("covid", "ny") |> check_data()

# User-provided data
my_df <- read.csv("my_data.csv") |> check_data()

# Then into the pipeline
df |> get_fcast(eval_start_date = "2025-01-01")
} # }
```
