#' Launch metaboliteR Shiny App
#'
#' Runs the Shiny Application for metaboliteR on your computer.
#'
#' @export
#'
launchApp <- function() {
  appDir <- system.file("shinyApp", package = "metaboliteR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `metaboliteR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
