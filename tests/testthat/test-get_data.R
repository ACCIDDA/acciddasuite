test_that("get_data validates pathogen argument", {
  expect_error(
    get_data(pathogen = "invalid"),
    "'arg' should be one of"
  )
})
