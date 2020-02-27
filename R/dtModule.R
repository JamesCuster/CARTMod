#' Add reactive datatable: UI function
#'
#' @inheritParams addEditUI
#' @param filterData hello
#'
#' @export
dtModuleUI <- function(id, filterData = NULL) {
  ns <- shiny::NS(id)

  # Create filter inputs if they exists
  if (!is.null(filterData)) {
    filters <-
      apply(
        filterData, 1,
        function(x) {
          if (x["ids"] == filterData$ids[1]) {
            style <- NULL
          } else {
            style <- "margin-left: 20px;"
          }
          shiny::div(
            shiny::selectizeInput(
              inputId = ns(x["ids"]),
              label = x["labels"],
              choices = "All"
            ),
            style = style
          )
        }
      )
  } else {
    filters <- NULL
  }

  list(
    shiny::div(
      filters,
      style = "display: flex; align-items: flex-start;"
    ),
    DT::dataTableOutput(ns("dt"))
  )
}



#' Add reactive datatable: server function
#'
#' @inheritParams addEdit
#' @inheritParams dtModuleUI
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
