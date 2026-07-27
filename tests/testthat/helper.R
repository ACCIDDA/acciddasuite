# Shared test fixtures.

#' Weekly surveillance data for tests
#'
#' Deterministic weekly counts per series (no RNG), one series per location
#' (crossed with `age_groups` when given). With `revisions = TRUE`, every week
#' is re-reported at delays of 0-2 weeks with increasing counts, snapshot as
#' of the latest week -- the right-truncated reporting-triangle shape
#' get_ncast() expects.
make_weekly_df <- function(locations = "NY",
                           n = 40,
                           start = as.Date("2023-01-01"),
                           target = "wk inc covid hosp",
                           age_groups = NULL,
                           revisions = FALSE) {
  cols <- list(
    target_end_date = start + 7 * (seq_len(n) - 1),
    location = locations
  )
  if (!is.null(age_groups)) {
    cols$age_group <- age_groups
  }
  df <- do.call(expand.grid, c(cols, stringsAsFactors = FALSE))

  series <- setdiff(names(df), "target_end_date")
  shift <- as.integer(factor(do.call(paste, df[series])))
  week <- as.integer(df$target_end_date - start) %/% 7L
  df$observation <- 100 + 10 * shift + 30 * sin(2 * pi * (week + 3 * shift) / 13)
  df$target <- target

  if (!revisions) {
    return(df)
  }
  reported <- do.call(rbind, lapply(0:2, function(delay) {
    out <- df
    out$as_of <- out$target_end_date + 7L * delay
    out$observation <- round(out$observation * (0.7 + 0.15 * delay))
    out
  }))
  reported[reported$as_of <= max(df$target_end_date), ]
}
