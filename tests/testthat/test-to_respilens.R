test_that("to_respilens throws error for wrong class", {
  expect_error(to_respilens(list(a = 1)), "Input must be an object of class 'accidda_cast'")
})

test_that("to_respilens throws error if multiple locations are present in either acciddacast object", {
  model_data <- data.frame(
    location = "US",
    reference_date = as.Date("2026-03-01"),
    target = "inc hosp",
    model_id = "test-model",
    horizon = 1,
    target_end_date = as.Date("2026-03-08"),
    output_type = "quantile",
    output_type_id = "0.5",
    value = 100
  )
  gt_data <- data.frame(
    location = "US",
    target_end_date = as.Date("2026-03-08"),
    target = "inc hosp",
    oracle_value = 105
  )
  
  # multiple locations in model_out_tbl
  bad_model_tbl <- rbind(
    model_data, 
    transform(model_data, location = "NY") 
  )
  cast_multi_model <- list(
    hubcast = list(model_out_tbl = bad_model_tbl, oracle_output = gt_data)
  )
  class(cast_multi_model) <- "accidda_cast"
  expect_error(
    to_respilens(cast_multi_model), 
    "Expected exactly one location in input data."
  )
  
  # multiple locations in oracle_output
  bad_oracle_tbl <- rbind(
    gt_data,
    transform(gt_data, location = "NY") 
  )
  cast_multi_oracle <- list(
    hubcast = list(model_out_tbl = model_data, oracle_output = bad_oracle_tbl)
  )
  class(cast_multi_oracle) <- "accidda_cast"
  expect_error(
    to_respilens(cast_multi_oracle), 
    "Expected exactly one location in input data."
  )
})

test_that("to_respilens creates correctly nested output", {
  ref_date <- "2026-03-01"
  target_val <- "inc hosp"
  model_name <- "test-model"
  horizon_val <- "1"
  model_data <- data.frame(
    location = "US",
    reference_date = as.Date(ref_date),
    target = target_val,
    model_id = model_name,
    horizon = as.numeric(horizon_val),
    target_end_date = as.Date("2026-03-08"),
    output_type = "quantile",
    output_type_id = "0.5",
    value = 100
  )
  gt_data <- data.frame(
    location = "US",
    target_end_date = as.Date("2026-03-08"),
    target = target_val,
    oracle_value = 105
  )
  fake_cast <- list(
    hubcast = list(model_out_tbl = model_data, oracle_output = gt_data)
  )
  class(fake_cast) <- "accidda_cast"

  result <- to_respilens(fake_cast)
  expect_named(result, c("metadata", "ground_truth", "forecasts"))
  
  # check nesting
  expect_type(result$metadata$hubverse_keys, "list")
  expect_true(model_name %in% result$metadata$hubverse_keys$models)
  expect_contains(names(result$forecasts), ref_date)
  expect_contains(names(result$forecasts[[ref_date]]), target_val)
  expect_contains(names(result$forecasts[[ref_date]][[target_val]]), model_name)
  model_entry <- result$forecasts[[ref_date]][[target_val]][[model_name]]
  expect_equal(model_entry$type, "quantile")
  expect_type(model_entry$predictions, "list")
  pred_entry <- model_entry$predictions[[horizon_val]]
  expect_equal(pred_entry$date, "2026-03-08")
  expect_equal(pred_entry$values, 100)
  expect_equal(pred_entry$quantiles, 0.5)
})

