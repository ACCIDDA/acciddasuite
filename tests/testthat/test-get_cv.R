test_that("get_cv rejects plain data frames", {
  df <- data.frame(wrong_column = 1)
  expect_error(
    get_cv(df, eval_start_date = Sys.Date()),
    "accidda_data or accidda_ncast"
  )
})

test_that("get_cv has correct default parameters", {
  fn_args <- formals(get_cv)
  expect_equal(fn_args$h, 4)
  expect_equal(fn_args$models, quote(default_models()))
  expect_equal(fn_args$step, quote(h))
})

test_that("default_models() returns the built-in fable model set", {
  m <- default_models()
  expect_type(m, "list")
  expect_named(m, c("NAIVE", "ETS", "THETA", "ARIMA"))
})

test_that("get_cv validates h parameter", {
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2020-01-01"), by = "week", length.out = 60),
    observation = rnorm(60, mean = 100, sd = 10),
    target = "wk inc covid hosp",
    location = "NY"
  ))
  expect_error(get_cv(df, eval_start_date = "2021-01-01", h = -1))
  expect_error(get_cv(df, eval_start_date = "2021-01-01", h = c(1, 2)))
})

test_that("get_cv runs on less than a year of data", {
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2023-01-01"), by = "week", length.out = 30),
    observation = rnorm(30, mean = 100, sd = 10),
    target = "wk inc covid hosp",
    location = "NY"
  ))
  cv <- get_cv(
    df,
    eval_start_date = "2023-04-01",
    h = 1,
    models = list(NAIVE = fable::NAIVE(observation), MEAN = fable::MEAN(observation)),
    step = 1
  )
  expect_s3_class(cv, "accidda_cv")
})

test_that("get_cv errors when eval_start_date leaves nothing to score", {
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2023-01-01"), by = "week", length.out = 30),
    observation = rnorm(30, mean = 100, sd = 10),
    target = "wk inc covid hosp",
    location = "NY"
  ))
  expect_error(
    get_cv(
      df,
      eval_start_date = as.Date("2023-01-01") + 7 * 29, # last observation
      h = 4,
      models = list(NAIVE = fable::NAIVE(observation))
    ),
    "too little to score"
  )
})

test_that("get_cv returns an accidda_cv with the expected structure", {
  set.seed(1)
  n <- 80
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2020-01-01"), by = "week", length.out = n),
    observation = 100 + 40 * sin(2 * pi * seq_len(n) / 52) + rnorm(n, 0, 5),
    target = "wk inc covid hosp",
    location = "NY"
  ))
  cv <- get_cv(df, eval_start_date = as.Date("2020-01-01") + 7 * 70, h = 4)

  expect_s3_class(cv, "accidda_cv")
  expect_named(cv, c("forecasts", "oracle", "score", "models", "meta", "data"))
  expect_true(all(c("NAIVE", "ETS", "THETA", "ARIMA") %in% names(cv$models)))
  expect_true(all(c("model_id", "wis") %in% names(cv$score)))
  expect_equal(cv$meta$h, 4)
})
