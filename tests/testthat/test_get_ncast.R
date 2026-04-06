# -----------------------------
# Helper: dummy accidda_data object
# -----------------------------
make_dummy_data <- function() {
  df <- data.frame(
    target_end_date = as.Date(c(
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
      "2025-01-29", "2025-02-05"
    )),
    observation = c(100, 120, 130, 80, 110, 50, 40, 100, 160, 20, 70),
    location = "test_loc",
    target = "cases"
  )

  structure(
    list(
      data = df,
      location = "test_loc",
      target = "cases",
      history = TRUE
    ),
    class = "accidda_data"
  )
}

# -----------------------------
# Test 1: week_floor behavior
# -----------------------------
test_that("week_floor correctly floors dates to week start", {
  x <- as.Date(c("2025-01-15", "2025-01-18"))

  result <- as.Date(cut(x, "week"))

  expect_equal(
    result,
    as.Date(c("2025-01-13", "2025-01-13"))
  )
})

# -----------------------------
# Test 2: transmute step
# -----------------------------
test_that("transmute creates correct columns", {
  df <- make_dummy_data()$data

  week_floor <- function(x) as.Date(cut(x, "week"))

  result <- df |>
    dplyr::transmute(
      reference_date = week_floor(target_end_date),
      report_date = week_floor(as_of),
      confirm = as.integer(round(observation))
    )

  expect_true(all(c("reference_date", "report_date", "confirm") %in% colnames(result)))
  expect_type(result$confirm, "integer")
})

# -----------------------------
# Test 3: summarise step
# -----------------------------
test_that("summarise keeps max confirm per group", {
  df <- data.frame(
    reference_date = as.Date(c("2025-01-01", "2025-01-01")),
    report_date = as.Date(c("2025-01-08", "2025-01-08")),
    confirm = c(100, 120)
  )

  result <- df |>
    dplyr::summarise(
      confirm = max(confirm, na.rm = TRUE),
      .by = c(reference_date, report_date)
    )

  expect_equal(nrow(result), 1)
  expect_equal(result$confirm, 120)
})

# -----------------------------
# Test 4: delta calculation
# -----------------------------
test_that("delta correctly computes increments", {
  df <- data.frame(
    reference_date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
    report_date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15")),
    confirm = c(100, 120, 130)
  )

  result <- df |>
    arrange(reference_date, report_date) |>
    group_by(reference_date) |>
    mutate(
      delta = confirm - dplyr::lag(confirm, default = 0L),
      count = pmax(0L, delta)
    ) |>
    ungroup()

  expect_equal(result$delta, c(100, 20, 10))
  expect_equal(result$count, c(100, 20, 10))
})

# -----------------------------
# Test 5: negative revisions handling
# -----------------------------
test_that("negative deltas are clamped to zero", {
  df <- data.frame(
    reference_date = as.Date(c("2025-01-01", "2025-01-01")),
    report_date = as.Date(c("2025-01-01", "2025-01-08")),
    confirm = c(120, 100)  # decrease (bad data)
  )

  result <- df |>
    arrange(reference_date, report_date) |>
    group_by(reference_date) |>
    mutate(
      delta = confirm - dplyr::lag(confirm, default = 0L),
      count = pmax(0L, delta)
    ) |>
    ungroup()

  expect_equal(result$count, c(120, 0))
})

# -----------------------------
# Test 6: full get_ncast basic output
# -----------------------------
test_that("get_ncast returns expected structure", {
  skip_if_not_installed("baselinenowcast")

  df <- make_dummy_data()

  result <- get_ncast(df, max_delay = 2, draws = 100, prop_delay = 0.5, scale_factor = 2)

  expect_true("data" %in% names(result))
  expect_true("plot" %in% names(result))
  expect_s3_class(result, "accidda_ncast")
})

# -----------------------------
# Test 7: corrected weeks only
# -----------------------------
test_that("only last max_delay weeks are corrected", {
  skip_if_not_installed("baselinenowcast")

  df <- make_dummy_data()

  result <- get_ncast(df, max_delay = 2, draws = 100, prop_delay = 0.5, scale_factor = 2)

  out <- result$data

  latest_date <- max(out$target_end_date)

  corrected_rows <- out$target_end_date > (latest_date - 7)

  expect_true(any(!is.na(out$ncast_lower[corrected_rows])))
  expect_false(all(is.na(out$ncast_lower[!corrected_rows])))
})

# -----------------------------
# Test 8: error when no history
# -----------------------------
test_that("error thrown when history is FALSE", {
  df <- make_dummy_data()
  df$history <- FALSE

  expect_error(get_ncast(df))
})

# -----------------------------
# Test 9: error when max_delay <= 0
# -----------------------------
test_that("error thrown for invalid max_delay", {
  df <- make_dummy_data()

  expect_error(get_ncast(df, max_delay = 0))
})
