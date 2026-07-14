library(testthat)

# -------------------------------
# Helper: create valid dummy data
# -------------------------------
make_valid_df <- function() {
  data.frame(
    target_end_date = c("2025-01-01", "2025-01-08", "2025-01-15",
                        "2025-01-01", "2025-01-08", "2025-01-15"),
    observation     = c(10, 20, 15, 10, 20, 20),
    location        = c("NY", "NY", "NY", "CA", "CA", "CA"),
    target          = c("cases", "cases", "cases", "cases", "cases", "cases"),
    stringsAsFactors = FALSE
  )
}

# -------------------------------
# 1. Basic success case
# -------------------------------
test_that("check_data works on valid input", {
  df <- make_valid_df()

  result <- check_data(df)

  expect_s3_class(result, "accidda_data")
  expect_equal(result$location, c("NY", "CA"))
  expect_equal(result$target, "cases")
  expect_false(result$history)

  expect_equal(result$window[["from"]], as.Date("2025-01-01"))
  expect_equal(result$window[["to"]], as.Date("2025-01-15"))
})

# -------------------------------
# 2. Already validated input
# -------------------------------
test_that("returns input if already accidda_data", {
  df <- make_valid_df()
  result1 <- check_data(df)

  result2 <- check_data(result1)

  expect_identical(result1, result2)
})

# -------------------------------
# 3. Input must be data frame
# -------------------------------
test_that("error if input is not a data frame", {
  expect_error(
    check_data(123),
    "`data` must be a data frame"
  )
})

# -------------------------------
# 4. Missing required columns
# -------------------------------
test_that("error when required columns are missing", {
  df <- data.frame(a = 1:3)

  expect_error(
    check_data(df),
    "Missing required columns"
  )
})

# -------------------------------
# 5. Type coercion works
# -------------------------------
test_that("type coercion converts columns correctly", {
  df <- make_valid_df()

  result <- check_data(df)

  expect_true(inherits(result$data$target_end_date, "Date"))
  expect_true(is.numeric(result$data$observation))
  expect_true(is.character(result$data$location))
  expect_true(is.character(result$data$target))
})

# -------------------------------
# 6. Invalid date should fail
# -------------------------------
test_that("invalid date causes error", {
  df <- make_valid_df()
  df$target_end_date <- as.Date(df$target_end_date)
  df$target_end_date[1] <- NA

  expect_error(
    check_data(df),
    "values that cannot be coerced to Date"
  )
})

# -------------------------------
# 7. Multiple targets should fail
# -------------------------------
test_that("multiple targets cause error", {
  df <- make_valid_df()
  df$target[2] <- "deaths"

  expect_error(
    check_data(df),
    "exactly one target"
  )
})

# -------------------------------
# 8. Multiple reporting intervals should fail
# -------------------------------
test_that("multiple reporting intervals cause error", {
  df <- make_valid_df()
  df$target_end_date[5:6] <- c("2025-01-16", "2025-01-31")
  expect_error(
    check_data(df),
    "inconsistent reporting intervals"
  )

  df$target_end_date[6] <- "2025-02-05"
  expect_error(
    check_data(df),
    "mixed reporting intervals"
  )

  df <- head(df, -2)
  expect_error(
    check_data(df),
    "at least two distinct `target_end_date` values"
  )
})

# -------------------------------
# 9. History detection
# -------------------------------
test_that("history is detected correctly", {
  df <- make_valid_df()
  df$as_of <- c("2025-01-02", "2025-01-03", "2025-01-04")

  result <- check_data(df)

  expect_true(result$history)
  expect_true(inherits(result$data$as_of, "Date"))
})

# -------------------------------
# 10. No history when as_of constant
# -------------------------------
test_that("history is FALSE when as_of has single value", {
  df <- make_valid_df()
  df$as_of <- c("2025-01-02", "2025-01-02", "2025-01-02")

  result <- check_data(df)

  expect_false(result$history)
})

# -------------------------------
# 11. Window calculation
# -------------------------------
test_that("window is computed correctly", {
  df <- make_valid_df()

  result <- check_data(df)

  expect_equal(result$window[["from"]], as.Date("2025-01-01"))
  expect_equal(result$window[["to"]], as.Date("2025-01-15"))
})

# -------------------------------
# 12. Print method works
# -------------------------------
test_that("print.accidda_data runs without error", {
  df <- make_valid_df()
  result <- check_data(df)

  expect_output(print(result), "Location")
  expect_output(print(result), "Target")
  expect_output(print(result), "Window")
  expect_output(print(result), "History")
  expect_output(print(result), "accidda_data")
})
