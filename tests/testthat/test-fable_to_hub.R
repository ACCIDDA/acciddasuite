hub_from <- function(df, key) {
  ts <- as_model_ts(df, key)
  fc <- ts |>
    fabletools::model(NAIVE = fable::NAIVE(observation)) |>
    fabletools::forecast(h = 2) |>
    dplyr::mutate(.id = 1L)
  fable_to_hub(fc, ts, key = key, target = "cases", interval = 7L)
}

test_that("fable_to_hub carries key columns into both flat hub tables", {
  df <- make_weekly_df(locations = c("NY", "CA"), n = 6)

  hub <- hub_from(df, "location")

  expect_setequal(unique(hub$model_out_tbl$location), c("NY", "CA"))
  expect_setequal(unique(hub$oracle_output$location), c("NY", "CA"))
  expect_contains(
    names(hub$model_out_tbl),
    c(
      "model_id", "reference_date", "target", "horizon", "location",
      "target_end_date", "output_type", "output_type_id", "value"
    )
  )
  # 2 locations x 2 horizons x 5 quantiles
  expect_equal(nrow(hub$model_out_tbl), 20)
  expect_equal(nrow(hub$oracle_output), 12)
})

test_that("fable_to_hub sets the origin and horizons per series", {
  df <- make_weekly_df(locations = c("NY", "CA"), n = 6)

  hub <- hub_from(df, "location")

  expect_equal(
    unique(hub$model_out_tbl$reference_date),
    max(df$target_end_date)
  )
  expect_setequal(unique(hub$model_out_tbl$horizon), 1:2)
})

test_that("fable_to_hub supports composite keys as task-ID columns", {
  df <- make_weekly_df(
    locations = c("NY", "CA"),
    age_groups = c("young", "old"),
    n = 4
  )

  hub <- hub_from(df, c("location", "age_group"))

  expect_true(
    all(c("location", "age_group") %in% names(hub$model_out_tbl))
  )
  expect_true(
    all(c("location", "age_group") %in% names(hub$oracle_output))
  )
  # 4 series x 2 horizons x 5 quantiles
  expect_equal(nrow(hub$model_out_tbl), 40)
})
