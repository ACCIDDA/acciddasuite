# Regenerates the packaged `example_data` dataset.
#
# Run from the package root. Needs a Delphi Epidata API key in the
# DELPHI_EPIDATA_KEY environment variable (see the epidatr package).
devtools::load_all()

example_data <- get_data(
  pathogen = "flu",
  geo_value = c("ny", "ca"),
  revisions = TRUE
)$data |>
  dplyr::filter(
    as_of <= as.Date("2025-12-14"),
    target_end_date >= as.Date("2022-06-01")
  )

usethis::use_data(example_data, overwrite = TRUE, compress = "xz")
