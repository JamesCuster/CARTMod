#' Adds the Add/Edit buttons and the associated header header
#'
#' @param id id argument given to module
#'
#' @examples
#' ui <- shiny::fluidPage(
#' addEditModuleUI("data")
#' )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shiny::shinyApp(ui, server)
#'
#' @export
#'
addEditModuleUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    shiny::tags$h1(id),
    shiny::actionButton(inputId = ns("add"), label = "Add"),
    shiny::actionButton(inputId = ns("edit"), label = "Edit"),
    shiny::tags$br(),
    shiny::tags$br()
  )
}


addEditModule <- function(input, output, session) {

}

