#' Create and display datatable: UI function
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



#' Create and display datatable: server function
#'
#' @inheritParams addEdit
#' @inheritParams dtModuleUI
#' @param filterData hello!
#'
#' @export
dtModule <- function(input, output, session, reactiveData, dbTable, filterData = NULL,
                      staticChoices = NULL) {
  # Build filter UI
  choices <- choicesReactive(inputData = filterData,
                             reactiveData = reactiveData,
                             staticChoices = staticChoices)

  output$dtFilters <- shiny::renderUI({
    ns <- session$ns
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
                choices = c("All", choices()[[x["ids"]]])
              ),
              style = style
            )
          }
        )
    } else {
      filters <- NULL
    }
    shiny::div(
      filters,
      style = "display: flex; align-items: flex-start;"
    )
  })

  # Used to presreve selected row on reloads if row is selected as well as
  # create the input that stores the table row identifier of the selected row to
  # be passed to other modules.
  # note: There is functionality inthe dtData reactive that allows for
  # preserving/clearing the selected row depending on whether the selection is
  # present in the filtered data.
  selected <- NULL
  shiny::observeEvent(c(input$dt_rows_selected, is.null(input$dt_rows_selected)), {
    selected <<- input$dt_rows_selected
    getSelectedRowIDMessage <-
      list(
        session$ns("dt_rows_selected_identifier"),
        dtData()[input$dt_rows_selected, 1]
      )
    session$sendCustomMessage("getSelectedRowID", getSelectedRowIDMessage)
  })


  # Data reactive to filter data
  dtData <- shiny::reactive({
    # Grab data frame
    df <- reactiveData[[dbTable]]

    if (!is.null(filterData)) {
      # Check filter inputs have been created
      shiny::req(
        unlist(
          lapply(filterData[, 1], function(x) input[[x]])
        )
      )
      # shiny::req(input[["speciesFilter"]], input[["smellFilter"]])


      # apply filters
      if (!is.null(filterData)) {
        for (i in 1:nrow(filterData)) {
          df <- apply(filterData[i, ], 1, applyFilters, .data = df, input = input)
          df <- as.data.frame(df, col.names = "")
        }
      }
    }


    # This handles the clearing of input[["dt_rows_selected"]] when the selected
    # row does not exist in the filtered data
    if (!is.null(shiny::isolate(input[["dt_rows_selected_identifier"]])) &&
        shiny::isolate(input[["dt_rows_selected_identifier"]]) %in% df[, 1]) {
      selected <<- which(df[, 1] == shiny::isolate(input[["dt_rows_selected_identifier"]]))
    }
    else {
      selected <<- NULL
    }
    dt_rows_selected_message <-
      list(
        session$ns("dt_rows_selected"),
        selected
      )
    session$sendCustomMessage("dt_rows_selected", dt_rows_selected_message)
    return(df)
  })

  # Creates the datatable
  output$dt <-
    DT::renderDataTable(
      DT::datatable(
        dtData(),
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


#' Apply filters to data displayed in dtModule
#'
#' @param .data the dataframe to be filtered and displayed in the dtModule
#' @param x a data frame row from filterData data.frame.
#' @inheritParams dtModule
#'
#' @export
applyFilters <- function(.data, x, input) {
    if (input[[x[["ids"]]]] != "All") {
      dplyr::filter(.data, eval(parse(text = x[["filterColumnIds"]])) == input[[x[["ids"]]]])
    }
    else {.data}
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
