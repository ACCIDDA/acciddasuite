test_that("get_fcast validates required columns", {
  df <- data.frame(wrong_column = 1)
  expect_error(
    get_fcast(df, eval_start_date = Sys.Date()),
    "target_end_date"
  )
})

test_that("get_fcast has correct default parameters", {
  fn_args <- formals(get_fcast)
  expect_equal(fn_args$h, 4)
  expect_equal(fn_args$top_n, 3)
  expect_null(fn_args$extra_models)
})

test_that("get_fcast validates h parameter", {
  df <- data.frame(
    target_end_date = seq(as.Date("2020-01-01"), by = "week", length.out = 60),
    observation = rnorm(60, mean = 100, sd = 10)
  )
  expect_error(get_fcast(df, eval_start_date = "2021-01-01", h = -1))
  expect_error(get_fcast(df, eval_start_date = "2021-01-01", h = c(1, 2)))
})

test_that("get_fcast requires at least 52 weeks of data before eval_start_date", {
  df <- data.frame(
    target_end_date = seq(as.Date("2023-01-01"), by = "week", length.out = 30),
    observation = rnorm(30, mean = 100, sd = 10)
  )
  expect_error(
    get_fcast(df, eval_start_date = "2023-08-01"),
    "At least 52 weeks"
  )
})
