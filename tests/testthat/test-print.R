test_that("print.accidda_cast handles accidda_cast objects", {
  # Create a minimal mock accidda_cast object
  mock_cast <- list(
    forecast = data.frame(
      target_end_date = as.Date(c("2024-01-01", "2024-01-08")),
      value = c(100, 110)
    ),
    score = data.frame(
      model_id = c("SNAIVE", "ETS"),
      wis = c(10.5, 12.3)
    ),
    plot = NULL
  )
  class(mock_cast) <- "accidda_cast"

  # Test that print doesn't error
  expect_output(print(mock_cast), "accidda_cast")
  expect_output(print(mock_cast), "Models evaluated")
  expect_output(print(mock_cast), "Forecast horizon")
})
