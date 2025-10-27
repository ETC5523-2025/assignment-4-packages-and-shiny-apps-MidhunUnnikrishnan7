# R/run_app.R

#' Launch the interactive app
#'
#' Opens the packaged Shiny app from `inst/app`.
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  appDir <- system.file("app", package = utils::packageName())
  if (!nzchar(appDir)) stop("App directory not found. Is the package installed?")
  shiny::runApp(appDir)
}
