#' Launch the Coral ID practice app
#'
#' Launches the interactive Coral ID practice app.
#'
#' Images are read from the external GitHub repository
#' \code{dohertyml/coralquiz-images}, using the structure
#' \code{<location>/<species_key>/<filename>} described in the accompanying
#' \code{index.csv} file.
#'
#' @param location Default location/project folder to use (for example
#'   \code{"caribbean_benthos"}). This must match a \code{source} value in the
#'   \code{index.csv} file in the \code{coralquiz-images} GitHub repository.
#'   If \code{NULL}, the first available location in \code{index.csv} is used.
#' @export
practice <- function(location = NULL) {
  run_coral_app(
    mode     = "practice",
    location = location,
    n        = NA_integer_,
    save_csv = FALSE,
    csv_path = NULL,
    user     = NULL
  )
}

#' Launch a graded quiz
#'
#' Launches a graded coral identification quiz in a Shiny app.
#'
#' Images are read from the external GitHub repository
#' \code{dohertyml/coralquiz-images}, using the structure
#' \code{<location>/<species_key>/<filename>} described in the accompanying
#' \code{index.csv} file. The app presents \code{n} questions and reports a
#' score. If requested, it also writes a one-line CSV summary of the attempt.
#'
#' @param location Location or project folder (for example
#'   \code{"caribbean_benthos"}). This must match a \code{source} value in the
#'   \code{index.csv} file in the \code{coralquiz-images} GitHub repository.
#' @param n Number of questions (default 20).
#' @param save_csv Logical; whether to write a CSV summary at the end.
#' @param csv_path Directory to write the CSV into (default working directory).
#' @param user Optional user name recorded in the CSV (default
#'   \code{Sys.info()[["user"]]}).
#' @export
quiz <- function(location,
                 n = 20,
                 save_csv = FALSE,
                 csv_path = getwd(),
                 user = Sys.info()[["user"]]) {
  stopifnot(length(n) == 1L, is.numeric(n), n >= 1)
  run_coral_app(
    mode     = "quiz",
    location = location,
    n        = as.integer(n),
    save_csv = isTRUE(save_csv),
    csv_path = csv_path,
    user     = user
  )
}

# Internal helper -------------------------------------------------------------

run_coral_app <- function(mode,
                          location = NULL,
                          n = NA_integer_,
                          save_csv = FALSE,
                          csv_path = NULL,
                          user = NULL) {
  # Find app dir whether installed or running from source
  app_dir_local <- file.path("inst", "app")
  if (file.exists(file.path(app_dir_local, "app.R"))) {
    app_dir <- normalizePath(app_dir_local, mustWork = TRUE)
  } else {
    app_dir <- system.file("app", package = "coralquiz")
  }
  if (nzchar(app_dir) == FALSE) {
    stop("Could not locate Shiny app in inst/app or in the installed package.")
  }

  # Pass run-time options used by inst/app/app.R
  old_opts <- options(
    coralquiz.mode           = mode,        # "practice" or "quiz"
    coralquiz.default_source = location,    # maps to `source` in index.csv
    coralquiz.n              = n,           # used only in quiz mode
    coralquiz.save_csv       = save_csv,
    coralquiz.csv_path       = csv_path,
    coralquiz.user           = user
  )

  on.exit(options(old_opts), add = TRUE)

  shiny::runApp(app_dir, launch.browser = TRUE)
}
