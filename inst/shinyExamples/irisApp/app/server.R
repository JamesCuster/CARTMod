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
                 "textInput"),
        stringsAsFactors = FALSE
    )

    callModule(addModule, "iris",
               modalTitle = "Add Iris",
               inputData = irisInputs,
               db = testdb,
               dbTable = "iris")
})
