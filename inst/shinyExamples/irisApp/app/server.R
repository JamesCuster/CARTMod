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

  irisUI <- function(ns, choices = NULL, values = NULL,
                     session = getDefaultReactiveDomain()) {
    if (is.null(values)) {
      modalInputs(ns = ns,
                  inputData = irisInputs,
                  choices = choices())
    }
    else {
      modalInputs2(ns = ns,
                   inputData = irisInputs,
                   choices = choices(),
                   values = values())
    }
  }



  callModule(addEdit, "iris",
             dtRow = reactive(input[["iris-dt_rows_selected_identifier"]]),
             addTitle = "Add Iris",
             editTitle = "Edit Iris",
             modalUI = irisUI,
             inputData = irisInputs,
             reactiveData = reactiveData,
             staticChoices = irisStaticChoices,
             dbTable = "iris",
             db = irisdb)


  callModule(dtModule, "iris",
             reactiveData,
             dbTable = "iris",
             filterData = irisFilters,
             staticChoices = irisStaticChoices)



  # Flowers -----------------------------------------------------------------
  flowersUI <- function(ns, choices = NULL, values = NULL,
                        session = getDefaultReactiveDomain()) {
    if (is.null(values)) {
      modalInputs(ns = ns,
                  inputData = flowerInputs,
                  choices = choices())
    }
    else {
      modalInputs2(ns = ns,
                   inputData = flowerInputs,
                   choices = choices(),
                   values = values())
    }
  }

  callModule(addEdit, "flowers",
             dtRow = reactive(input[["flowers-dt_rows_selected_identifier"]]),
             addTitle = "Add Flowers",
             editTitle = "Edit Flowers",
             modalUI = flowersUI,
             inputData = flowerInputs,
             reactiveData = reactiveData,
             checkDuplicate = c("flowerName", "flowerName2"),
             dbTable = "flowers",
             db = irisdb)

  callModule(dtModule, "flowers",
             reactiveData,
             dbTable = "flowers")
})
