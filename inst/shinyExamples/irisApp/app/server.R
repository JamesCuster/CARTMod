#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # 1.1 Monitor Database ------------------------------------------------------
  monitorDatabase <-
    reactivePoll(
      intervalMillis = 1000,
      session,
      checkFunc = function() {
        modifiedCurrent <- tbl(irisdb, "modified") %>%
          collect() %>%
          as.data.frame(stringsAsfactors = FALSE)
        if (as.POSIXct(modified$modified) <
              as.POSIXct(modifiedCurrent$modified)) {
          modified <<- modifiedCurrent
          return(TRUE)
        } else {
          return(FALSE)
        }
      },
      valueFunc = function() {
        loadDatabase(irisdb, tables = modified$tableName)
      }
    )

  # observe which applies the monitorDatabase reactive
  observe({
    monitorDatabase()
  })

  # iris --------------------------------------------------------------------
  irisInputs <- data.frame(
    ids = c("irisID", names(iris)),
    labels = c("irisID", gsub("\\.", " ", names(iris))),
    type = c("skip",
             "textInput",
             "textInput",
             "textInput",
             "textInput",
             "selectizeInput"),
    choicesTable = c(NA, NA, NA, NA, NA, "flowers"),
    choicesValues = c(NA, NA, NA, NA, NA, "flowerID"),
    choicesLabels = c(NA, NA, NA, NA, NA, "flowerName"),
    stringsAsFactors = FALSE
  )

  callModule(addModule, "iris",
             modalTitle = "Add Iris",
             inputData = irisInputs,
             db = irisdb,
             dbTable = "iris",
             reactiveData = reactiveData)

  callModule(dtModule, "iris",
             tab = reactiveData$iris)



  # Flowers -----------------------------------------------------------------
  flowerInputs <- data.frame(
    ids = c("flowerID", "flowerName"),
    labels = c("flowerID", "Flower Name"),
    type = c("skip", "textInput"),
    stringsAsFactors = FALSE
  )

  callModule(addModule, "flowers",
             modalTitle = "Add Flower",
             inputData = flowerInputs,
             db = irisdb,
             dbTable = "flowers",
             reactiveData = reactiveData)

  flowersRowSelected <- NULL

  output$flowers <-
    renderDataTable(
      datatable(
        reactiveData$flowers,
        selection = list(
          mode = "single",
          selected = flowersRowSelected
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
})
