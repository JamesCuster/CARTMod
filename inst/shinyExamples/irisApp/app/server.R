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
  irisUI <- function(ns, choices = NULL) {
    # Both options below work, which is what I wanted.
    modalInputs(ns,
                inputData = irisInputs,
                choices = choices())
    # list(
    #   selectizeInput(ns("Species"), "Species",
    #                  choices = reactiveData$flowers$flowerName),
    #   selectizeInput(ns("smell"), "Smell", choices = irisStaticChoices$smell)
    # )
  }
  callModule(addModule, "iris",
             modalTitle = "Add Iris",
             modalUI = irisUI,
             inputData = irisInputs,
             reactiveData = reactiveData,
             staticChoices = irisStaticChoices,
             dbTable = "iris",
             db = irisdb)

  callModule(dtModule, "iris",
             reactiveData,
             dbTable = "iris")



  # Flowers -----------------------------------------------------------------
  flowersUI <- function(ns, choices = NULL) {
    modalInputs(ns,
                inputData = flowerInputs,
                choices = choices)
  }

  callModule(addModule, "flowers",
             modalTitle = "Add Flowers",
             modalUI = flowersUI,
             inputData = flowerInputs,
             reactiveData = reactiveData,
             checkDuplicate = c("flowerName"),
             dbTable = "flowers",
             db = irisdb)


  callModule(dtModule, "flowers",
             reactiveData,
             dbTable = "flowers")
})
