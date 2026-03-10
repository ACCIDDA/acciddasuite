# Convert accida_cast to RespiLens format

Convert accida_cast to RespiLens format

## Usage

``` r
to_respilens(accida_cast, path = NULL)
```

## Arguments

- accida_cast:

  An object of class `accida_cast`, the output of
  [`get_fcast()`](https://accidda.github.io/acciddasuite/reference/get_fcast.md).

- path:

  Optional file path to write the JSON output to. Must end with `.json`.
  If `NULL`, the output is not written to disk.

## Value

A named list with a single metadata JSON structure and one JSON
structure per location.
