#' Run complexr application
#'
#' Launches the Shiny app bundled within the package.
#'
#' @return Launches a Shiny application.
#' @export
run_app <- function() {

  if (shiny::isRunning()) {
    stop("A Shiny app is already running in this session.")
  }

  app_dir <- system.file("app", package = "complexr")

  if (app_dir == "") {
    stop("Could not find Shiny app")
  }

  shiny::runApp(app_dir, launch.browser = TRUE)
}
