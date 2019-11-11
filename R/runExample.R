#' Run example app using all functionality
#'
#' This app uses the \code{iris} and \code{mtcars} data to showcase the
#' \code{CARTMod} modules and functions in action.
#'
#' @export
runExample <- function() {
  appDir <- system.file("shinyExamples", "irisApp", "app", package = "CARTMod")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CARTMod`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
