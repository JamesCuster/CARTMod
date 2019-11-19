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



# Datatable module --------------------------------------------------------

dtModuleUI <- function(id) {
  ns <- NS(id)

  list(
    dataTableOutput(ns("dt"))
  )
}

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



dtModule <- function(input, output, session, reactiveData, tab) {
  output$dt <-
    renderDataTable(
      datatable(
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
