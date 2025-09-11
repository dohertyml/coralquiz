# Launch the app using a local path or an external photos root
practice <- function(source = NULL, photos_root = NULL) {
  # Prefer local project path
  local_app <- file.path("inst", "app")
  if (file.exists(file.path(local_app, "app.R"))) {
    app_dir <- normalizePath(local_app)
  } else {
    # Fallback to installed package, if you later make this a package
    app_dir <- system.file("app", package = "coralquiz")
    if (app_dir == "") stop("Could not find app in inst/app or in an installed package")
  }

  # Pass defaults to the app
  options(coralquiz.default_source = source)
  options(coralquiz.photos_root   = photos_root)

  shiny::runApp(app_dir, launch.browser = TRUE)
}
