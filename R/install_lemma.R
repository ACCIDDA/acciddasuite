#' Install LEMMA-forecast
#'
#' One-time setup for [LEMMA()]. Clones the LEMMA-forecast repository into a
#' user cache directory and installs its Python dependencies into a dedicated
#' reticulate virtualenv. The path is stored in
#' `options(acciddasuite.lemma_path)` so subsequent sessions find it.
#'
#' @param path Directory for the clone. Defaults to a per-user cache.
#' @return Invisibly, the absolute path of the clone.
#' @export
#' @examples
#' \dontrun{
#' install_lemma()
#' getOption("acciddasuite.lemma_path")
#' #"/Users/cy/Library/Application Support/org.R-project.R/R/acciddasuite/LEMMA-forecast"
#' list.files(getOption("acciddasuite.lemma_path"))
#' }
install_lemma <- function(
  path = file.path(tools::R_user_dir("acciddasuite", "data"), "LEMMA-forecast")
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "Install 'reticulate' first: install.packages('reticulate').",
      call. = FALSE
    )
  }
  if (!nzchar(Sys.which("git"))) {
    stop("`git` was not found on PATH.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(path)) {
    repo <- "https://github.com/ACCIDDA/LEMMA-forecast.git"
    if (system2("git", c("clone", repo, shQuote(path))) != 0L) {
      stop("git clone failed.", call. = FALSE)
    }
  }

  envname <- "acciddasuite-lemma"
  if (!reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_create(envname, version = ">=3.12")
  }
  reticulate::virtualenv_install(
    envname,
    requirements = file.path(path, "requirements.txt")
  )

  options(acciddasuite.lemma_path = path)
  message("LEMMA-forecast ready at ", path)
  invisible(path)
}


#' Resolve the LEMMA-forecast clone path
#'
#' Checks `options(acciddasuite.lemma_path)` first, then the default cache
#' directory populated by [install_lemma()]. Errors if neither exists.
#'
#' @keywords internal
#' @noRd
.lemma_path <- function() {
  p <- getOption("acciddasuite.lemma_path")
  if (is.null(p) || !nzchar(p)) {
    p <- file.path(tools::R_user_dir("acciddasuite", "data"), "LEMMA-forecast")
  }
  if (!dir.exists(p)) {
    stop(
      "LEMMA-forecast not found. Run `acciddasuite::install_lemma()` first.",
      call. = FALSE
    )
  }
  p
}
