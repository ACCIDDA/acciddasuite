# devtools::load_all()
df <- get_data(pathogen = "rsv", geo_values = "ny")
extra_models = list(
    EPIESTIM = EPIESTIM(
        observation,
        mean_si = 4,
        std_si = 3
    )
)

x = get_fcast(
    df,
    eval_start_date = "2025-09-01",
    h = 3,
    top_n = 5,
    extra_models = extra_models
)
to_respilens(x) |> jsonlite::write_json("respi.json", auto_unbox = TRUE)

x
x$plot
x$hubcast
x$score

library(ggplot2)
x$hubcast$model_out_tbl |>
    filter(output_type_id %in% c("0.025", "0.5", "0.975")) |>
    tidyr::pivot_wider(names_from = output_type_id, values_from = value) |>
    ggplot(aes(x = target_end_date, group = model_id)) +
    geom_ribbon(
        aes(ymin = `0.025`, ymax = `0.975`, fill = model_id),
        alpha = 0.2
    ) +
    geom_line(aes(y = `0.5`, colour = model_id), linetype = "solid") +
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
    ) +
    labs(x = "Target end date", y = "Value") +
    theme_classic()
