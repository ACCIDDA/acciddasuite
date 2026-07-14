test_that("get_fcast rejects plain data frames", {
  df <- data.frame(wrong_column = 1)
  expect_error(
    get_fcast(df),
    "accidda_data or accidda_ncast"
  )
})

test_that("get_fcast has correct default parameters", {
  fn_args <- formals(get_fcast)
  expect_equal(fn_args$h, 4)
  expect_equal(fn_args$top_n, 3)
  expect_equal(fn_args$models, quote(default_models()))
})

test_that("get_fcast builds a hub forecast and ensemble without cross-validation", {
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2020-01-01"), by = "week", length.out = 60),
    observation = 50 + 10 * sin(2 * pi * seq_len(60) / 8),
    target = "wk inc covid hosp",
    location = c("NY", "CA")
  ))
  fcast <- get_fcast(
    df,
    models = list(
      NAIVE = fable::NAIVE(observation),
      DRIFT = fable::RW(observation ~ drift())
    ),
    h = 2
  )
  expect_s3_class(fcast, "accidda_fcast")
  expect_null(fcast$score)
  expect_true(all(c("NAIVE", "DRIFT") %in% fcast$meta$models))
  expect_true("ENSEMBLE" %in% fcast$hub$model_out_tbl$model_id)
  expect_true(all(is.finite(fcast$hub$model_out_tbl$value)))
})

test_that("get_fcast reuses the cv's top_n models when models is not supplied", {
  set.seed(1)
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2023-01-01"), by = "week", length.out = 40),
    observation = 100 + 5 * sin(2 * pi * seq_len(40) / 8) + rnorm(40, 0, 2),
    target = "wk inc covid hosp",
    location = c("NY", "CA")
  ))
  cv <- get_cv(
    df,
    eval_start_date = "2023-06-01",
    h = 1,
    models = list(NAIVE = fable::NAIVE(observation), MEAN = fable::MEAN(observation)),
    step = 1
  )
  fcast <- cv |> get_fcast(top_n = 1)
  expect_equal(fcast$meta$top_n, 1)
  expect_length(fcast$meta$models, 1)
  expect_true(fcast$meta$models %in% c("NAIVE", "MEAN"))
})

test_that("get_fcast uses explicit models over the cv ranking when supplied", {
  set.seed(1)
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2023-01-01"), by = "week", length.out = 40),
    observation = 100 + 5 * sin(2 * pi * seq_len(40) / 8) + rnorm(40, 0, 2),
    target = "wk inc covid hosp",
    location = c("NY", "CA")
  ))
  cv <- get_cv(
    df,
    eval_start_date = "2023-06-01",
    h = 1,
    models = list(NAIVE = fable::NAIVE(observation), MEAN = fable::MEAN(observation)),
    step = 1
  )
  # The CV evaluated NAIVE/MEAN; forecast with a different set that includes a
  # model (DRIFT) the CV never saw.
  fcast <- cv |>
    get_fcast(
      h = 2,
      models = list(
        MEAN = fable::MEAN(observation),
        DRIFT = fable::RW(observation ~ drift())
      )
    )
  expect_s3_class(fcast, "accidda_fcast")
  expect_true(all(c("MEAN", "DRIFT") %in% fcast$meta$models))
  expect_false("NAIVE" %in% fcast$meta$models) # NAIVE was in the cv, not the forecast set
  expect_equal(fcast$meta$top_n, 2)
  # the cross-validation ranking is still retained
  expect_false(is.null(fcast$score))
})

test_that("get_fcast validates h parameter", {
  df <- check_data(data.frame(
    target_end_date = seq(as.Date("2020-01-01"), by = "week", length.out = 80),
    observation = rpois(80, lambda = 100),
    target = "wk inc covid hosp",
    location = c("NY", "CA")
  ))
  expect_error(get_fcast(df, h = -1))
  expect_error(get_fcast(df, h = c(1, 2)))
})

test_that("get_fcast returns expected structure", {
  df <- check_data(data.frame(
    target_end_date = as.Date(c(
      "2025-01-01", "2025-01-01", "2025-01-01",
      "2025-01-08", "2025-01-08",
      "2025-01-15",
      "2025-01-22", "2025-01-22", "2025-01-22",
      "2025-01-29", "2025-01-29",
      "2025-01-01", "2025-01-01", "2025-01-01",
      "2025-01-08", "2025-01-08",
      "2025-01-15",
      "2025-01-22", "2025-01-22", "2025-01-22",
      "2025-01-29", "2025-01-29"
    )),
    as_of = as.Date(c(
      "2025-01-01", "2025-01-08", "2025-01-15",
      "2025-01-08", "2025-01-15",
      "2025-01-15",
      "2025-01-22", "2025-01-29", "2025-02-05",
      "2025-01-29", "2025-02-05",
      "2025-01-01", "2025-01-08", "2025-01-15",
      "2025-01-08", "2025-01-15",
      "2025-01-15",
      "2025-01-22", "2025-01-29", "2025-02-05",
      "2025-01-29", "2025-02-05"
    )),
    observation = c(100, 120, 130, 80, 110, 50, 40, 100, 160, 20, 70,
                    110, 130, 130, 90, 100, 60, 80, 110, 160, 30, 80),
    location = c("test_loc1", "test_loc1", "test_loc1", "test_loc1",
                 "test_loc1", "test_loc1", "test_loc1", "test_loc1",
                 "test_loc1", "test_loc1", "test_loc1",
                 "test_loc2", "test_loc2", "test_loc2", "test_loc2",
                 "test_loc2", "test_loc2", "test_loc2", "test_loc2",
                 "test_loc2", "test_loc2", "test_loc2"),
    target = "wk inc covid hosp"
  ))

  print(df)

  result <- get_fcast(df)

  expect_s3_class(result, "accidda_fcast")
  expect_equal(names(result$hub), c("test_loc1", "test_loc2"))
  expect_equal(result$meta$location, c("test_loc1", "test_loc2"))
  expect_equal(names(result$meta$nowcast), c("test_loc1", "test_loc2"))
})
