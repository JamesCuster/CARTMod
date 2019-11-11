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
shinyServer(function(input, output) {
  # iris --------------------------------------------------------------------
    irisInputs <- data.frame(
        ids = names(iris),
        labels = gsub("\\.", " ", names(iris)),
        type = c("textInput",
                 "textInput",
                 "textInput",
                 "textInput",
                 "selectizeInput"),
        choicesTable = c(NA, NA, NA, NA, "flowers"),
        choicesValues = c(NA, NA, NA, NA, "flowerID"),
        choicesLabels = c(NA, NA, NA, NA, "flowerName"),
        stringsAsFactors = FALSE
    )






    addModule <- function(input, output, session,
                          modalTitle, inputData, db, dbTable) {
        # Reactive which gathers the choices for the select and selectize inputs
        choicesReactive <- reactive({
            browser()
            choices <-
                apply(
                    inputData, 1,
                    function(x) {
                        if (grepl("select", x["type"])) {
                            valueLabel(
                                df = reactiveData[[x["choicesTable"]]],
                                value = x["choicesValues"],
                                label = x["choicesLabels"])
                        } else {
                            return(NA)
                        }
                    }
                )
            choices <- setNames(choices, inputData$ids)
            return(choices)
        })

        # controls what happens when add is pressed
        shiny::observeEvent(input$add, {
            browser()
            choices <- choicesReactive()
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







    callModule(addModule, "iris",
               modalTitle = "Add Iris",
               inputData = irisInputs,
               db = testdb,
               dbTable = "iris")



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
               db = testdb,
               dbTable = "flowers")
})
