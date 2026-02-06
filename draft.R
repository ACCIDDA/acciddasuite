devtools::load_all()
df <- get_data(pathogen = "rsv", geo_values = "ny")
df
eval_start_date = "2025-09-01"
h = 4
top_n = 3
extra_models = NULL

x = get_fcast(df, eval_start_date)
x
x$plot
x$hubcast
x$score

library(ggplot2)
x$hubcast$model_out_tbl |>
    ggplot(aes(
        x = target_end_date,
        y = value,
        color = model_id,
        group = interaction(model_id, reference_date, output_type_id)
    )) +
    geom_line(aes(lty = output_type_id)) +
    geom_vline(aes(xintercept = reference_date), linetype = "dashed") +
    geom_vline(
        xintercept = as.Date(
            x$hubcast$oracle_output |> pull(target_end_date) |> max()
        ),
        color = "red",
        size = 1
    ) +
    geom_line(
        data = x$hubcast$oracle_output |>
            filter(target_end_date >= eval_start_date),
        aes(x = target_end_date, y = oracle_value),
        group = 1,
        color = "black",
        size = 1
    )
