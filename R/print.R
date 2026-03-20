#' Print an accidda_fcast object
#'
#' Displays a concise summary of an \code{accidda_fcast} object,
#' including model scores, forecast horizon, and hub format contents.
#'
#' @param x An object of class \code{accidda_fcast}.
#' @param ... Additional arguments (currently ignored).
#' @export
print.accidda_fcast <- function(x, ...) {
  cat("<accidda_fcast>\n\n")

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

#' Print an accidda_ncast object
#'
#' Displays a concise summary of an \code{accidda_ncast} object.
#'
#' @param x An object of class \code{accidda_ncast}.
#' @param ... Additional arguments (currently ignored).
#' @export
print.accidda_ncast <- function(x, ...) {
  cat("<accidda_ncast>\n\n")

  corrected <- !is.na(x$data$ncast_lower)
  n_corrected <- sum(corrected)
  rng <- range(x$data$target_end_date[corrected])
  cat(
    "Nowcasted", n_corrected, "weeks:",
    as.character(rng[1]), "to", as.character(rng[2]), "\n"
  )

  cat("\n$data  corrected series (", nrow(x$data), " rows)\n", sep = "")
  cat("$plot  nowcast visualisation\n")

  invisible(x)
}
