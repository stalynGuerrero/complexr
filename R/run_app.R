#' Run complexr application
#'
#' Launches the Shiny app bundled within the package in the default browser.
#'
#' @return Called for its side effect: opens the Shiny application. Returns
#'   invisibly when the app is stopped.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
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
