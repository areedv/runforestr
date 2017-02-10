fluidPage(
  fluidRow(
    column(width = 3,
           numericInput(inputId = "n", "Sample Size", value = 25)
           ),
    column(width = 6,
           leaflet::leafletOutput(outputId = "map")
           ),
    column(width = 3)
  ),
  fluidRow(
    column(width = 12,
           plotlyOutput(outputId = "plot")
           )
  )
)
