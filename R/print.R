#' Print an accidda_cast object
#'
#' Displays a concise summary of an \code{accidda_cast} object,
#' including model scores, forecast horizon, and hub format contents.
#'
#' @param x An object of class \code{accidda_cast}.
#' @param ... Additional arguments (currently ignored).
#' @export
print.accidda_cast <- function(x, ...) {
  cat("<accidda_cast>\n\n")

  cat("Models evaluated:\n")
  print(x$score |> dplyr::select(model_id, wis), row.names = FALSE)

  cat("\nForecast horizon:\n")
  rng <- range(x$hubcast$model_out_tbl$target_end_date)
  cat("  From:", as.character(rng[1]), "\n")
  cat("  To:  ", as.character(rng[2]), "\n")

  cat("\nContents:\n")
  cat("  $hubcast   hub forecast object\n")
  cat("  $score     model ranking table\n")
  cat("  $plot      ggplot2 object\n")

  invisible(x)
}
