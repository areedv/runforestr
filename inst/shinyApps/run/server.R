function(input, output) {
  output$map <- leaflet::renderLeaflet({
    r_map(t$data)
  })
  output$plot <- plotly::renderPlotly({
    r_time(t$data)
  })
}
