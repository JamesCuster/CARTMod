shinyUI(
  fluidPage(
    tabsetPanel(
      # iris --------------------------------------------------------------
      tabPanel(
        "Iris",

        # Add edit Iris
        addEditUI("iris"),

        # Datatable
        dtModuleUI("iris")
      ),

      # flowers ----------------------------------------------------------------
      tabPanel(
        "Flowers",

        # Add edit flowers
        addEditUI("flowers"),

        # Datatable
        dtModuleUI("flowers")
      )
    )
  )
)
