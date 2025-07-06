#' Launch Coral Genus Flashcard Quiz
#'
#' @param dataset Folder name containing images (e.g., "onetree")
#' @export
launch_quiz <- function(dataset = NULL) {
  if (is.null(dataset)) {
    stop("Please specify the dataset folder name, e.g., launch_quiz('onetree')")
  }

  # Set environment variable so app.R can read it
  Sys.setenv(CORALQUIZ_DATASET = dataset)

  app_dir <- system.file("shinyapp", package = "coralquiz")

  shiny::runApp(
    appDir = app_dir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}
