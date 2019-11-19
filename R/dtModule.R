#' Add reactive datatable: UI function
#'
#' @inheritParams addModuleUI
#'
#' @export
dtModuleUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    DT::dataTableOutput(ns("dt"))
  )
}



#' Add reactive datatable: server function
#'
#' @inheritParams addModule
#'
#' @export
dtModule <- function(input, output, session, reactiveData, dbTable) {
  # used to presreve selected row on reloads if row is selected
  selected <- NULL
  shiny::observeEvent(input$dt_rows_selected, {
    selected <<- input$dt_rows_selected
  })

  output$dt <-
    DT::renderDataTable(
      DT::datatable(
        reactiveData[[dbTable]],
        selection = list(
          mode = "single",
          selected = selected
        ),
        rownames = FALSE,
        options = list(
          dom = '<"top"fl> t <"bottom"ip>',
          order = list(0, "desc")
        )
      ),
      server = TRUE
    )
}

# # Alternative approach to dtModule function. Kept as comment just in case it
# # is needed for future development
# dtModule <- function(input, output, session, dbTable) {
#   output$dt <-
#     renderDataTable(
#       datatable(
#         dbTable(),
#         selection = list(
#           mode = "single",
#           selected = input[[paste0(session$ns("dt"), "_rows_selected")]]
#         ),
#         rownames = FALSE,
#         options = list(
#           dom = '<"top"fl> t <"bottom"ip>',
#           rowId = "researcherID",
#           order = list(0, "desc")
#         )
#       ),
#       server = TRUE
#     )
# }
#
# callModule(dtModule, "iris",
#            dbTable = reactive(reactiveData$iris))
