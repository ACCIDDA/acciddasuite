test_that("print.accidda_fcast handles accidda_fcast objects", {
  mock_cast <- structure(
    list(
      hub = list(
        model_out_tbl = data.frame(
          target_end_date = as.Date(c("2024-01-01", "2024-01-08"))
        ),
        oracle_output = data.frame()
      ),
      score = data.frame(
        model_id = c("SNAIVE", "ETS"),
        wis = c(10.5, 12.3)
      ),
      meta = list(
        models = c("SNAIVE", "ETS"),
        top_n = 2,
        h = 4,
        location = "NY",
        nowcast = FALSE
      )
    ),
    class = "accidda_fcast"
  )

  expect_output(print(mock_cast), "accidda_fcast")
  expect_output(print(mock_cast), "Models ranked")
  expect_output(print(mock_cast), "Forecast horizon")
})

test_that("print.accidda_fcast handles a NULL score", {
  mock_cast <- structure(
    list(
      hub = list(
        model_out_tbl = data.frame(
          target_end_date = as.Date(c("2024-01-01", "2024-01-08"))
        ),
        oracle_output = data.frame()
      ),
      score = NULL,
      meta = list(models = c("ETS", "ARIMA"), top_n = 2, h = 4,
                  location = "NY", nowcast = FALSE)
    ),
    class = "accidda_fcast"
  )

  expect_output(print(mock_cast), "Models forecast")
})

test_that("print.accidda_cv handles accidda_cv objects", {
  mock_cv <- structure(
    list(
      forecasts = data.frame(),
      oracle = data.frame(),
      score = data.frame(model_id = c("ETS", "ARIMA"), wis = c(5, 7)),
      models = list(ETS = NULL, ARIMA = NULL),
      meta = list(eval_start_date = as.Date("2025-01-01"), h = 4,
                  location = "NY")
    ),
    class = "accidda_cv"
  )

  expect_output(print(mock_cv), "accidda_cv")
  expect_output(print(mock_cv), "Models ranked")
})
