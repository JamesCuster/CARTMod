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
        stringsAsFactors = FALSE
    )

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
