library(shiny)
library(RSQLite)
library(dplyr)
library(CARTMod)

# Connect to database
dbPath <- system.file("shinyExamples", "irisApp", "iris.sqlite", package = "CARTMod")
testdb <- dbConnect(dbDriver("SQLite"), dbPath)

# Create reactive to store db tables
reactiveData <- reactiveValues()


# Function that loads db tables and stores in irisReactive
loadData <- function(db, tables = c("iris", "mtcars", "modified")) {
  if ("iris" %in% tables) {
    reactiveData$iris <- tbl(db, "iris") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("mtcars" %in% tables) {
    reactiveData$mtcars <- tbl(db, "mtcars") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
  if ("modified" %in% tables) {
    modified <<- tbl(db, "modified") %>%
      collect() %>%
      as.data.frame(stringsAsFactors = FALSE)
  }
}

loadData(testdb)



addModuleUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    shiny::actionButton(inputId = ns("add"), label = "Add")
  )
}





addModule <- function(input, output, session, modalTitle, inputData, db, dbTable) {
  # controls what happens when add is pressed
  shiny::observeEvent(input$add, {
    browser()
    shiny::showModal(
      shiny::modalDialog(
        title = modalTitle,
        modalInputs(
          session,
          inputData
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













insertCallback <- function(input, output, session, ids, db, tab) {
  # Creates data.frame of field values for new entry
  new <- lapply(ids,
                function(x) {
                  if (class(input[[x]]) == "Date") {
                    if (length(input[[x]]) == 0) {
                      NA
                    }
                    else {
                      as.character(input[[x]])
                    }
                  }
                  else if (is.null(input[[x]]) || length(input[[x]]) == 0 || input[[x]] == "") {
                    NA
                  }
                  else {
                    input[[x]]
                  }
                }) %>%
    setNames(ids) %>%
    as.data.frame()

  # inserts new entry into database
  dbWriteTable(db, tab, new, append = TRUE)
}
