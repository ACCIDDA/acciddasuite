#' Print an \code{accidda_data} object
#' @param x An \code{accidda_data} object.
#' @param ... Ignored.
#' @export
print.accidda_data <- function(x, ...) {
  cat("<accidda_data>\n\n")
  cat("Location:", x$location, "\n")
  cat("Target:  ", x$target, "\n")
  cat(
    "Window:  ",
    as.character(x$window["from"]),
    "to",
    as.character(x$window["to"]),
    "(",
    nrow(x$data[!duplicated(x$data$target_end_date), ]),
    "dates )\n"
  )
  cat("Interval:", x$interval, "days\n")
  if (x$history) {
    as_of_rng <- range(x$data$as_of)
    cat(
      "History: ",
      "TRUE (",
      as.character(as_of_rng[1]),
      "to",
      as.character(as_of_rng[2]),
      ")\n"
    )
  } else {
    cat("History:  FALSE\n")
  }
  invisible(x)
}

#' Print an \code{accidda_ncast} object
#' @param x An \code{accidda_ncast} object.
#' @param ... Ignored.
#' @export
print.accidda_ncast <- function(x, ...) {
  cat("<accidda_ncast>\n\n")
  cat("Location:", x$location, "\n")
  cat("Target:  ", x$target, "\n")

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

#' Print an \code{accidda_cv} object
#' @param x An \code{accidda_cv} object.
#' @param ... Ignored.
#' @export
print.accidda_cv <- function(x, ...) {
  cat("<accidda_cv>\n\n")

  cat("Models ranked (cross-validation):\n")
  print(x$score |> dplyr::select(model_id, wis), row.names = FALSE)

  cat(
    "\nEvaluated from", as.character(x$meta$eval_start_date),
    "| horizon", x$meta$h, "weeks |", x$meta$location, "\n"
  )

  cat("\nContents:\n")
  cat("  $forecasts  per-origin forecasts (model_out_tbl)\n")
  cat("  $oracle     observed truth (oracle_output)\n")
  cat("  $score      model ranking table\n")
  cat("  $models     model specifications\n")
  cat("  $meta       eval_start_date, h, location, target, interval\n")

  invisible(x)
}

#' Print an \code{accidda_fcast} object
#' @param x An \code{accidda_fcast} object.
#' @param ... Ignored.
#' @export
print.accidda_fcast <- function(x, ...) {
  cat("<accidda_fcast>\n\n")

  if (is.null(x$score)) {
    cat("Models forecast:", paste(x$meta$models, collapse = ", "), "\n")
  } else {
    cat("Models ranked (cross-validation):\n")
    print(x$score |> dplyr::select(model_id, wis), row.names = FALSE)
  }

  cat("\nForecast horizon:\n")
  rng <- range(x$hub$model_out_tbl$target_end_date)
  cat("  From:", as.character(rng[1]), "\n")
  cat("  To:  ", as.character(rng[2]), "\n")

  cat("\nContents:\n")
  cat("  $hub    hub forecast object (model_out_tbl, oracle_output)\n")
  cat("  $score  model ranking table, or NULL\n")
  cat("  $meta   models, top_n, h, location, target, interval, nowcast\n")

  invisible(x)
}
