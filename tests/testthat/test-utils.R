test_that("as_model_ts builds one keyed tsibble across series", {
  df <- make_weekly_df(locations = c("NY", "CA"), n = 5)

  ts <- as_model_ts(df, "location")

  expect_s3_class(ts, "tbl_ts")
  expect_equal(tsibble::key_vars(ts), "location")
  expect_equal(nrow(ts), 10)
})

test_that("as_model_ts fills gaps within each series only", {
  df <- make_weekly_df(locations = c("NY", "CA"), n = 5)
  gap <- df$location == "NY" & df$target_end_date == as.Date("2023-01-15")

  ts <- as_model_ts(df[!gap, ], "location")

  expect_equal(nrow(ts), 10) # implicit gap becomes an explicit NA row
  expect_true(is.na(
    ts$observation[ts$location == "NY" & ts$target_end_date == as.Date("2023-01-15")]
  ))
  expect_false(anyNA(ts$observation[ts$location == "CA"]))
})

test_that("as_model_ts supports composite keys", {
  df <- make_weekly_df(
    locations = c("NY", "CA"),
    age_groups = c("young", "old"),
    n = 4
  )

  ts <- as_model_ts(df, c("location", "age_group"))

  expect_equal(tsibble::key_vars(ts), c("location", "age_group"))
  expect_equal(nrow(ts), 16)
})
