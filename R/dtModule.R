#' Add reactive datatable: UI function
#'
#' @export
dtModuleUI <- function(id) {
  ns <- NS(id)

  list(
    DT::dataTableOutput(ns("dt"))
  )
}



#' Add reactive datatable: server function
#'
#' @export
dtModule <- function(input, output, session, reactiveData, tab) {
  output$dt <-
    DT::renderDataTable(
      DT::datatable(
        reactiveData[[tab]],
        selection = list(
          mode = "single",
          selected = input[[paste0(session$ns("dt"), "_rows_selected")]]
        ),
        rownames = FALSE,
        options = list(
          dom = '<"top"fl> t <"bottom"ip>',
          rowId = "researcherID",
          order = list(0, "desc")
        )
      ),
      server = TRUE
    )
}

# # Alternative approach to dtModule function. Kept as comment just in case it
# # is needed for future development
# dtModule <- function(input, output, session, tab) {
#   output$dt <-
#     renderDataTable(
#       datatable(
#         tab(),
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
#            tab = reactive(reactiveData$iris))
