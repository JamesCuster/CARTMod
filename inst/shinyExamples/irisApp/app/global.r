library(shiny)
library(RSQLite)
library(dplyr)
library(DT)
library(CARTMod)

# Connect to database
dbPath <- system.file("shinyExamples", "irisApp", "iris.sqlite",
                      package = "CARTMod")
irisdb <- dbConnect(dbDriver("SQLite"), dbPath)

# Create reactive to store db tables
reactiveData <- reactiveValues()


# Function that loads db tables and stores in irisReactive
loadDatabase <- function(db, tables = c("iris", "flowers", "modified")) {
  if ("iris" %in% tables) {
    reactiveData$iris <- tbl(db, "iris") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("flowers" %in% tables) {
    reactiveData$flowers <- tbl(db, "flowers") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("modified" %in% tables) {
    modified <<- tbl(db, "modified") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
}

loadDatabase(irisdb)






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
          div(
            selectizeInput(
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
    div(
      filters,
      style = "display: flex; align-items: flex-start;"
    ),
    DT::dataTableOutput(ns("dt"))
  )
}


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

  # # updates filter choices as new data is added.
  # observeEvent(lapply(names(reactiveValuesToList(reactiveData)), function(x) {reactiveData[[x]]}), {
  #   browser()
  #   # If no filterData is provided, want to skip this observeEvent
  #   if (is.null(filterData)) {
  #     return()
  #   }
  #
  #   # Gather choices for filters
  #   choices <- choicesReactive(filterData, reactiveData)
  #   apply(
  #     filterData, 1,
  #     function(x) {
        # updateSelectizeInput(
        #   session = session,
        #   inputId = x["ids"],
        #   choices = c(All = "All", choices()[[x["ids"]]]),
        #   selected = input[["ids"]]
        # )
  #     }
  #   )
  # })
  if (!is.null(filterData)) {
    dtFilterUpdates(input, output, session, filterData = filterData, reactiveData = reactiveData)
  }
}



dtFilterUpdates <- function(input, output, session, filterData, reactiveData) {
  filtersList <- split(filterData, filterData$choicesTable)
  lapply(
    filtersList,
    function(x) {
      observeEvent(reactiveData[[x$choicesTable[1]]], {
        choices <- choicesReactive(x, reactiveData)
        apply(x, 1,
          function(y) {
            updateSelectizeInput(
              session = session,
              inputId = y["ids"],
              choices = c(All = "All", choices[[y["ids"]]]),
              selected = input[[y["ids"]]]
            )
          }
        )
      })
    }
  )
}


choicesReactive <- function(inputData, reactiveData) {
  choicesReact <- shiny::reactive({
    choices <-
      lapply(
        inputData$ids,
        function(x) {
          if (grepl("select", inputData[inputData$ids == x, "type"])) {
            valueLabel(
              df = reactiveData[[inputData[inputData$ids == x, "choicesTable"]]],
              value = inputData[inputData$ids == x, "choicesValues"],
              label = inputData[inputData$ids == x, "choicesLabels"])
          } else {
            return(NA)
          }
        }
      )
    choices <- stats::setNames(choices, inputData$ids)
    return(choices)
  })
  return(choicesReact())
}



# irisFilters <- data.frame(
#   ids = c("species", "species", "species"),
#   labels = c("Species", "species", "species"),
#   type = c("selectizeInput", "selectizeInput", "selectizeInput"),
#   choicesTable = c("flowers", "iris", "flowers"),
#   choicesValues = c("flowerID", "flowerID", "flowerID"),
#   choicesLabels = c("flowerName", "flowerName", "flowerName"),
#   stringsAsFactors = FALSE
# )

# filtersList <- split(irisFilters, irisFilters$choicesTable)
# lapply(
#   filtersList,
#   function(x) {
#     browser()
#     observeEvent(reactiveData[[x$choicesTable[1]]], {
#       apply(x, 1,
#         function(y) {
#           browser()
#           updateSelectizeInput(
#             session = session,
#             inputId = y["ids"],
#             choices = c(All = "All", choices[[y["ids"]]]),
#             selected = input[[y["ids"]]]
#           )
#         }
#         )
#     })
#   }
# )
