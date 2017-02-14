function(input, output, session) {
  output$map <- leaflet::renderLeaflet({
    k <- r_map(t$data)
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::clearBounds() %>%
      leaflet::addPolylines(k$data$LongitudeDegrees, k$data$LatitudeDegrees)
    m
  })
  output$plot <- plotly::renderPlotly({
    l <- r_time(t$data)

    # set y axis
    # pulse
    first_y <- list(title = "Heart rate", overlaying = "n", side = "left",
                    zeroline = FALSE, range = l$y1, showgrid = FALSE)

    # elevation
    second_y <- list(title = "", overlaying = "y", side = "right",
                     zeroline = FALSE, range = l$y2, showgrid = FALSE,
                     showticklabels = FALSE)
    # pace
    third_y <- list(title = "",
                    overlaying = "y", side = "right",
                    zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                    range = l$y3)

    p <- plotly::plot_ly(x = ~ l$x_data, mode = "lines", source = "trackpoint")
    # test with pulse zones
    for (i in 1:(length(l$zones) - 1)) {
      p <- plotly::add_trace(p, y = rep(l$zones[i + 1], length(l$x_data)),
                             name = paste0("I", as.character(i)),
                             mode = "none", type = "scatter", yaxis = "y1",
                             fillcolor = l$pzc[i + 1], fill = "tonexty",
                             hoverinfo = "text")
    }

    p <- plotly::add_trace(p, y = ~ l$data$FakeAltitudeMeters,
                           name = "Elevation", type = "scatter", mode = "none",
                           fill ="tozeroy", yaxis = "y2",
                           fillcolor = "rgba(190, 190, 190, 0.3)",
                           text = paste("Alt:", l$data$AltitudeMeters),
                           hoverinfo = "text") %>%

      plotly::add_trace(y = ~ l$data$HeartRateBpm,
                        type = "scatter", mode = "none", name = "HR",
                        line = list(color = "rgb(0, 0, 0)", dash = "dot"),
                        yaxis = "y1", fill = "tozeroy",
                        fillcolor = "rgba(255, 255, 255, 1)",
                        text = paste("HR:", l$data$HeartRateBpm),
                        hoverinfo = "text") %>%

      plotly::add_trace(y = ~ l$data$Pace, name = "Pace", type = "scatter",
                        mode = "line", yaxis = "y3",
                        text = paste("Pace:", as.difftime(l$data$Pace,
                                                          units = "mins")),
                        hoverinfo = "text") %>%
      plotly::layout(yaxis = first_y, yaxis2 = second_y, yaxis3 = third_y,
                     legend = list(orientation = "h"))
    p

  })

  output$hover <- renderPrint({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) "Test failed" else d
  })
}
