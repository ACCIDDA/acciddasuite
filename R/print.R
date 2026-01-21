#' Print an accida_cast object
#'
#' Displays a concise summary of an \code{accida_cast} object,
#' including model scores, forecast horizon, and available contents.
#'
#' @param acast An object of class \code{accida_cast}.
#' @param ... Additional arguments (currently ignored).
#' @export
print.accida_cast <- function(acast, ...) {
  cat("<accida_cast>\n\n")

  cat("Models evaluated:\n")
  print(acast$score |> dplyr::select(.model, CRPS), row.names = FALSE)

  cat("\nForecast horizon:\n")
  rng <- range(acast$forecast$target_end_date)
  cat("  From:", as.character(rng[1]), "\n")
  cat("  To:  ", as.character(rng[2]), "\n")

  cat("\nContents:\n")
  cat("  $forecast  fable forecast object\n")
  cat("  $score     model ranking table\n")
  cat("  $plot      ggplot2 object\n")

  invisible(acast)
}
