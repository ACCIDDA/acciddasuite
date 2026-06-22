library(tidyverse)

ebola <- read.csv(
  "https://raw.githubusercontent.com/scc-usc/ebola2026/refs/heads/main/hubverse_observed_data.csv"
) |>
  filter(
    target == "insp_sitrep__national_cumulative_confirmed_deaths__daily"
  ) |>
  mutate(target_end_date = as.Date(target_end_date)) |>
  complete(
    target_end_date = seq(
      min(target_end_date),
      max(target_end_date),
      by = "day"
    )
  ) |>
  fill(target, location, .direction = "downup") |>
  mutate(observation = zoo::na.approx(observation, na.rm = FALSE))

ebola

ebola |>
  ggplot(aes(x = target_end_date, y = observation, group = 1)) +
  geom_point() +
  geom_line()


devtools::load_all()
# pak::pak("ACCIDDA/acciddasuite")

fc <- check_data(ebola) |>
  get_cv(
    eval_start_date = "2026-06-01",
    h = 7,
    step = 1
  ) |>
  get_fcast(
    h = 7,
    models = c(
      default_models(),
      list(
        PROPHET = fable.prophet::prophet(log(observation)),
        NNETAR = fable::NNETAR(log(observation))
      )
    )
  )


# Quantiles arrive long (one row per level); pivot wide to ymin/ymax columns.
fc_wide <- fc$hub$model_out_tbl |>
  filter(output_type == "quantile") |>
  select(model_id, target_end_date, output_type_id, value) |>
  pivot_wider(names_from = output_type_id, values_from = value)

# Observed history (the hub oracle) and the last observed point.
obs <- fc$hub$oracle_output |>
  select(target_end_date, observation = oracle_value)
last_obs <- obs |> slice_max(target_end_date, n = 1)

# Connect the last observation to each model's median forecast.
median_path <- bind_rows(
  tidyr::expand_grid(
    model_id = unique(fc_wide$model_id),
    last_obs |> rename(`0.5` = observation)
  ),
  fc_wide |> select(model_id, target_end_date, `0.5`)
) |>
  arrange(model_id, target_end_date)

ggplot() +
  # 50% and 95% forecast intervals (ribbons show as fans when h > 1).
  geom_ribbon(
    data = fc_wide,
    aes(target_end_date, ymin = `0.025`, ymax = `0.975`),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_ribbon(
    data = fc_wide,
    aes(target_end_date, ymin = `0.25`, ymax = `0.75`),
    fill = "steelblue",
    alpha = 0.35
  ) +
  # Observed history.
  geom_line(data = obs, aes(target_end_date, observation)) +
  geom_point(data = obs, aes(target_end_date, observation)) +
  # Forecast: median path, 50% (thick) and 95% (thin) intervals at each horizon.
  geom_line(
    data = median_path,
    aes(target_end_date, `0.5`),
    colour = "steelblue"
  ) +
  geom_vline(
    xintercept = last_obs$target_end_date,
    linetype = "dashed",
    colour = "grey60"
  ) +
  facet_wrap(~model_id) +
  labs(
    title = "Ebola cumulative confirmed deaths (DRC)",
    subtitle = "Median with 50% and 95% intervals",
    x = NULL,
    y = "Cumulative confirmed deaths"
  ) +
  theme_minimal()
