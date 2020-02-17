# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tabsetPanel(
      # iris --------------------------------------------------------------
      tabPanel(
        "Iris",

        # Add button
        addModuleUI("iris"),

        # Datatable
        dtModuleUI("iris")
      ),

      # flowers
      tabPanel(
        "Flowers",

        # Add button
        addModuleUI("flowers"),

        # Datatable
        dtModuleUI("flowers")
      )
    )
  )
)
