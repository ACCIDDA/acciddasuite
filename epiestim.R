library(tidyverse)
library(fable)
library(tsibble)
library(ggplot2)
library(fabletools)
library(incidence)
library(EpiEstim)
library(projections)

devtools::load_all()
set.seed(123)
df <- get_data(pathogen = "covid", geo_values = "ny")
y = get_fcast(
  df,
  eval_start_date = "2025-11-29",
  top_n = 4,
  h = 4 # 4 weeks ahead
) |>
  pipetime::time_pipe("fcasting")
y
y$score
y$plot

daily_incid <- df |>
  select(target_end_date, observation) |>
  # slice off the last row due to right censoring
  slice_head(n = -1) |>
  mutate(count = observation / 7) |>
  uncount(7, .id = "day_num") |>
  mutate(date = target_end_date - (7 - day_num)) |>
  (\(x) as.incidence(x$count, dates = x$date, interval = 1))()
daily_incid |> plot()


# Estimate Rt over the last 14 days
Rt <- EpiEstim::estimate_R(
  incid = daily_incid,
  method = "parametric_si",
  config = EpiEstim::make_config(list(
    mean_si = 3.96,
    std_si = 4.75,
    t_start = length(daily_incid$counts) - 14,
    t_end = length(daily_incid$counts)
  ))
)
plot(Rt)
h = 4
proj <- projections::project(
  x = daily_incid,
  R = pluck(Rt, "R", "Median(R)"),
  si = pluck(Rt, "si_distr")[-1],
  n_sim = 100,
  R_fix_within = TRUE,
  n_days = 7 * h
)
proj |> str()
plot(proj)

plot(daily_incid) |>
  add_projections(proj, boxplots = FALSE, quantiles = c(0.25, 0.75)) +
  scale_x_date(
    limits = c(max(daily_incid$dates) - 365, max(daily_incid$dates) + 30)
  ) +
  theme_classic()

####
# library(hubEvals)
# library(hubExamples)
# library(hubUtils)

# f = y$forecast$observation[1]
# f |> quantile(p = c(0.25, 0.5, 0.75))
