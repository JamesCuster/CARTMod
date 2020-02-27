#' Add reactive datatable: UI function
#'
#' @inheritParams addEditUI
#'
#' @export
dtModuleUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    # JS to handle setting the row identifier for selected row in DT as an input
    # value to be passed to the edit function so that the correct row is used to
    # populate the edit modal
    shiny::tags$script("
      Shiny.addCustomMessageHandler('getSelectedRowID', function(value) {
      Shiny.setInputValue(value[0], value[1]);
      });
    "),
    # JS to preserve or remove selected row after filter depending on if
    # selected row is in filtered data
    shiny::tags$script("
      Shiny.addCustomMessageHandler('dt_rows_selected', function(value) {
      Shiny.setInputValue(value[0], value[1]);
      });
    "),
    shiny::uiOutput(ns("dtFilters")),
    DT::dataTableOutput(ns("dt"))
  )
}



#' Add reactive datatable: server function
#'
#' @inheritParams addEdit
#' @inheritParams dtModuleUI
#' @param filterData hello!
#'
#' @export
dtModule <- function(input, output, session, reactiveData, dbTable, filterData = NULL) {
  # used to presreve selected row on reloads if row is selected
  selected <- NULL
  shiny::observeEvent(input$dt_rows_selected, {
    selected <<- input$dt_rows_selected
  })

  # Creates the datatable
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

  if (!is.null(filterData)) {
    dtFilterUpdates(input, output, session, filterData = filterData, reactiveData = reactiveData)
  }
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


#' Update filter choices
#'
#' @inheritParams addEdit
#' @param filterData Hello
#'
#' @export
dtFilterUpdates <- function(input, output, session, filterData, reactiveData) {

  filtersList <- split(filterData, filterData$choicesTable)
  lapply(
    filtersList,
    function(x) {

      shiny::observeEvent(reactiveData[[x$choicesTable[1]]], {
        choices <- choicesReactive(x, reactiveData)
        apply(x, 1,
              function(y) {

                shiny::updateSelectizeInput(
                  session = session,
                  inputId = y["ids"],
                  choices = c(All = "All", choices()[[y["ids"]]]),
                  selected = input[[y["ids"]]]
                )
              }
        )
      })
    }
  )
}
