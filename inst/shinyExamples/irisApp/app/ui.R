# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tabsetPanel(
      # iris
      tabPanel(
        "Iris",
        addModuleUI("iris")
      ),

      # flowers
      tabPanel(
        "Flowers",
        addModuleUI("flowers")
      )
    )
  )
)
