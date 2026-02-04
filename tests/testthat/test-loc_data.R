test_that("loc_data dataset exists and has expected structure", {
  expect_true(exists("loc_data", envir = asNamespace("acciddasuite")))

  data("loc_data", package = "acciddasuite")

  expect_s3_class(loc_data, "data.frame")
  expect_true("abbreviation" %in% names(loc_data))
  expect_true("location" %in% names(loc_data))
  expect_true("location_name" %in% names(loc_data))
})
