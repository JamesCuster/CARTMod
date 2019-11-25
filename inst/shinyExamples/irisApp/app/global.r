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


# iris --------------------------------------------------------------------
irisInputs <- data.frame(
  ids = c("irisID", names(iris), "smell"),
  labels = c("irisID", gsub("\\.", " ", names(iris)), "Smell"),
  type = c("skip",
           "textInput",
           "textInput",
           "textInput",
           "textInput",
           "selectizeInput",
           "selectizeInput"),
  choicesTable = c(NA, NA, NA, NA, NA, "flowers", "static"),
  choicesValues = c(NA, NA, NA, NA, NA, "flowerID", NA),
  choicesLabels = c(NA, NA, NA, NA, NA, "flowerName", NA),
  stringsAsFactors = FALSE
)

irisStaticChoices <- list(
  smell = c("Poor", "Fair", "Good"),
  smellFilter = c("Poor", "Fair", "Good")
)


irisFilters <- data.frame(
  ids = c("speciesFilter", "smellFilter"),
  labels = c("Species", "Smell"),
  type = c("selectizeInput", "selectInput"),
  choicesTable = c("flowers", "static"),
  choicesValues = c("flowerID", NA),
  choicesLabels = c("flowerName", NA),
  stringsAsFactors = FALSE
)


# Flowers -----------------------------------------------------------------
flowerInputs <- data.frame(
  ids = c("flowerID", "flowerName"),
  labels = c("flowerID", "Flower Name"),
  type = c("skip", "textInput"),
  stringsAsFactors = FALSE
)





dtModuleUI <- function(id, filterData = NULL, staticChoices = NULL) {
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
              choices = c("All", staticChoices[[x["ids"]]])
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


dtModule <- function(input, output, session, reactiveData, dbTable, filterData = NULL, staticChoices) {
  # used to presreve selected row on reloads if row is selected
  selected <- NULL
  shiny::observeEvent(input$dt_rows_selected, {
    selected <<- input$dt_rows_selected
  })



  filterReactive <- reactive({
    # If there is no filter data, return that reactiveData dbTable as is
    if (is.null(filterData)) {
      return(reactiveData[[dbTable]])
    }

    # If there is filter data, apply the filters
    filtered <- reactiveData[[dbTable]] %>%
      {
        if (input[[filterData$ids[1]]] != "All") {
          filter(., Species == input[[filterData$ids[1]]])
        } else {
          .
        }
      } %>%
      {
        if (input[[filterData$ids[2]]] != "All") {
          filter(., smell == input[[filterData$ids[2]]])
        } else {
          .
        }
      }
    return(filtered)
  })

  # Creates the datatable
  output$dt <-
    DT::renderDataTable(
      DT::datatable(
        filterReactive(),
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


addModule <- function(input, output, session,
                      modalTitle, inputData, db, dbTable, reactiveData,
                      staticChoices = NULL) {
  # controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    # Checks inputData for select input types, if present, gathers the choices
    if (any(grepl("select", inputData$type))) {
      choices <- choicesReactive(inputData, reactiveData, staticChoices)
    }

    # Creates modal
    shiny::showModal(
      shiny::modalDialog(
        title = modalTitle,
        modalInputs(
          session = session,
          inputData = inputData,
          choices = choices
        ),
        footer =
          list(
            shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("insert"), "Save")
          )
      )
    )
  })

  # Controls what happens when Save is pressed
  shiny::observeEvent(input$insert, {
    insertCallback(input, output, session, inputData$ids, db, dbTable)
    shiny::removeModal()
  })
}

choicesReactive <- function(inputData, reactiveData, staticChoices) {
  choicesReact <- shiny::reactive({
    choices <-
      lapply(
        inputData$ids,
        function(x) {
          if (grepl("select", inputData[inputData$ids == x, "type"])) {
            if (tolower(inputData[inputData$ids == x, "choicesTable"]) == "static") {
              staticChoices[[x]]
            } else {
              valueLabel(
                df = reactiveData[[inputData[inputData$ids == x, "choicesTable"]]],
                value = inputData[inputData$ids == x, "choicesValues"],
                label = inputData[inputData$ids == x, "choicesLabels"])
            }
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
