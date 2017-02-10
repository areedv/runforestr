function(input, output) {
  output$map <- leaflet::renderLeaflet({
    r_map(t$data)
  })
  output$plot <- renderPlotly({
    r_time(t$data)
  })
}
