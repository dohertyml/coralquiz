#' Launch the Coral ID practice app
#'
#' Reads images from bundled `inst/app/www/photos/<source>/<species>/...`
#' or from an external `photos_root` with the same structure.
#' @param source Default location/project folder to use (e.g., "caribbean_benthos")
#' @param photos_root Optional external folder containing sources and species subfolders
#' @export
practice <- function(source = NULL, photos_root = NULL) {
  run_coral_app(
    mode        = "practice",
    source      = source,
    n           = NA_integer_,
    photos_root = photos_root,
    save_csv    = FALSE,
    csv_path    = NULL,
    user        = NULL
  )
}

#' Launch a graded quiz
#'
#' Presents n questions and reports a score. Optionally writes a CSV summary.
#' @param source Location/project folder (e.g., "caribbean_benthos")
#' @param n Number of questions (default 20)
#' @param photos_root Optional external folder of images
#' @param save_csv Write a CSV summary at the end
#' @param csv_path Directory to write the CSV into (default working directory)
#' @param user Optional user name recorded in the CSV
#' @export
quiz <- function(source,
                 n = 20,
                 photos_root = NULL,
                 save_csv = FALSE,
                 csv_path = getwd(),
                 user = Sys.info()[["user"]]) {
  stopifnot(length(n) == 1L, is.numeric(n), n >= 1)
  run_coral_app(
    mode        = "quiz",
    source      = source,
    n           = as.integer(n),
    photos_root = photos_root,
    save_csv    = isTRUE(save_csv),
    csv_path    = csv_path,
    user        = user
  )
}

# Internal helper -------------------------------------------------------------

run_coral_app <- function(mode,
                          source = NULL,
                          n = NA_integer_,
                          photos_root = NULL,
                          save_csv = FALSE,
                          csv_path = NULL,
                          user = NULL) {
  # Find app dir whether installed or running from source
  app_dir_local <- file.path("inst", "app")
  if (file.exists(file.path(app_dir_local, "app.R"))) {
    app_dir <- normalizePath(app_dir_local, mustWork = TRUE)
  } else {
    app_dir <- system.file("app", package = "coralQuiz")
  }
  if (nzchar(app_dir) == FALSE) {
    stop("Could not locate Shiny app in inst/app or in the installed package.")
  }

  # Pass run-time options
  options(
    coralquiz.mode        = mode,        # "practice" or "quiz"
    coralquiz.default_source = source,
    coralquiz.n           = n,           # used only in quiz mode
    coralquiz.photos_root = photos_root, # optional external library
    coralquiz.save_csv    = save_csv,
    coralquiz.csv_path    = csv_path,
    coralquiz.user        = user
  )

  shiny::runApp(app_dir, launch.browser = TRUE)
}
