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
  callModule(addModule, "iris",
             modalTitle = "Add Iris",
             inputData = irisInputs,
             db = irisdb,
             dbTable = "iris",
             reactiveData = reactiveData)

  callModule(dtModule, "iris",
             reactiveData,
             dbTable = "iris",
             filterData = irisFilters
             )



  # Flowers -----------------------------------------------------------------
  callModule(addModule, "flowers",
             modalTitle = "Add Flower",
             inputData = flowerInputs,
             db = irisdb,
             dbTable = "flowers",
             reactiveData = reactiveData,
             checkDuplicate = c("flowerName", "flowerName2"))

  callModule(dtModule, "flowers",
             reactiveData,
             dbTable = "flowers")
})
