library(magrittr)
library(plotly)

function(input, output, session) {

  # get the data
  k <- r_map(t$data)
  k <- dplyr::transmute(k, lng = LongitudeDegrees, lat = LatitudeDegrees)


  # reactive trackpoint hover data
  th <- reactive({
    k <- dplyr::mutate(k, key = as.integer(row.names(k)))
    k <- dplyr::filter(k, key == hk())
    k <- dplyr::select(k, lng, lat)
    k
  })

  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::clearBounds() %>%
      leaflet::addPolylines(k$lng, k$lat)
    # shift view nw
    lngs <- m$x$limits$lng
    lats <- m$x$limits$lat
    m <- leaflet::fitBounds(m, lng1 = lngs[1], lat1 = lats[1] - diff(lats),
                            lng2 = lngs[2] + diff(lngs), lat2 = lats[2])
    m
  })

  observe({
    marker <- th()
    # clear all markers when empty hover data
    if (dim(marker)[1] == 0) {
      leaflet::leafletProxy("map", data = marker) %>%
        leaflet::clearGroup("markers")
    } else {
      leaflet::leafletProxy("map", data = marker) %>%
        leaflet::addCircleMarkers(radius = 7, weight = 1, color = "#777777",
                                  fillColor = "#999999", fillOpacity = 0.3,
                                  group = "markers")
    }
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

    p <- plotly::plot_ly(x = ~ l$x_data, #Smode = "lines",
                         key = row.names(l$data), source = "trackpoint")
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
                        type = "scatter", mode = "lines", name = "HR",
                        line = list(color = "rgb(0, 0, 0)", dash = "dot"),
                        yaxis = "y1", fill = "tozeroy",
                        fillcolor = "rgba(255, 255, 255, 1)",
                        text = paste("HR:", l$data$HeartRateBpm),
                        hoverinfo = "text") %>%

      plotly::add_trace(y = ~ l$data$Pace, name = "Pace", mode = "lines",
                        type = "scatter", yaxis = "y3",
                        line = list(color = "rgba(0, 0 , 255, 0.7"),
                        text = paste("Pace:", as.difftime(l$data$Pace,
                                                          units = "mins")),
                        hoverinfo = "text") %>%
      plotly::layout(yaxis = first_y, yaxis2 = second_y, yaxis3 = third_y,
                     legend = list(orientation = "h"))
    p

  })

  hk <- renderText({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) -1 else as.integer(d$key[1])
  })
}
