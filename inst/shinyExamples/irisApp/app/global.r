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
