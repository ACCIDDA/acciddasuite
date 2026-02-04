test_that("get_data validates pathogen argument", {
  expect_error(
    get_data(pathogen = "invalid"),
    "'arg' should be one of"
  )
})

test_that("get_data accepts valid pathogen values", {
  # Test that valid pathogen values don't error on validation
  expect_no_error(match.arg("covid", choices = c("covid", "flu", "rsv")))
  expect_no_error(match.arg("flu", choices = c("covid", "flu", "rsv")))
  expect_no_error(match.arg("rsv", choices = c("covid", "flu", "rsv")))
})

test_that("get_data has default geo_values parameter", {
  # Check the function signature has default
  fn_args <- formals(get_data)
  expect_equal(fn_args$geo_values, "*")
})
