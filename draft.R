library(purrr)

df <- get_data(
  "covid",
  "ny"
) |>
  pluck("data")
#filter(target_end_date >= "2024-01-01")
# LEMMA(
#   formula,
#   approach            = "Flatline",
#   ensemble_method     = "Random Forest",
#   quantiles           = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
#   timesteps_per_bin   = NULL,        # NULL = auto-detect from tsibble
#   reference_date      = NULL,        # NULL = LEMMA default
#   season_start_date   = NULL,        # NULL = LEMMA default
#   hyperparams         = list(),      # named list, approach-specific overrides
#   n_sim               = 1000L
# )

extra_models <- list(
  LEMMA_FL = LEMMA(observation, approach = "Flatline"),
  LEMMA_AR = LEMMA(observation, approach = "ARIMA"),
  LEMMA_SIKJ = LEMMA(observation, approach = "SIKJalpha"),
  LEMMA_SIKJ_TUNED = LEMMA(
    observation,
    approach = "SIKJalpha",
    hyperparams = list(
      halpha_list = seq(0.98, 0.90, by = -0.02),
      un_list = c(50, 100),
      hk = 2L,
      hjp = 7L
    )
  ),
  LEMMA_ARIMA_TUNED = LEMMA(
    observation,
    approach = "ARIMA",
    hyperparams = list(
      arima_autoregressive_orders = c(3, 7, 14),
      arima_differencing_orders = c(0, 1)
    )
  )
)

cv <-
  check_data(df) |>
  get_cv(
    eval_start_date = as.Date(max(df$target_end_date) - 100),
    h = 4,
    models = c(default_models(), extra_models)
  ) |>
  pipetime::time_pipe("LEMMA", log = "fcasting")
cv$score

fcast <- get_fcast(cv, top_n = 10)
to_respilens(fcast, "respilens.json")
