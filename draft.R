df <- get_data(pathogen = "covid", geo_values = "ny")
df
eval_start_date = "2025-09-01"
h = 4
top_n = 3
extra_models = NULL

x = get_fcast(df, eval_start_date)
x$plot
x$forecast
x$score
x
