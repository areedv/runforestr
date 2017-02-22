library(magrittr)
library(plotly)
library(runforestr)

function(input, output, session) {

  # get the data
  k <- r_map(t$data)
  k <- dplyr::transmute(k, lng = LongitudeDegrees, lat = LatitudeDegrees)

  l <- r_time(t$data)


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

  output$distribution_lap_plot <- plotly::renderPlotly({

    if (input$lap_type == "equal distance"){
      laps <- make_laps_distance(l$data$Time, l$data$DistanceMeters,
                                 convert_factor = 1000)
    } else {
      laps <- t$meta$Laps
    }

    id <- make_intesity_distribution(laps, l$data$HeartRateBpm, l$data$Time,
                                    l$zones)

    # yaxis, range also including upper and lower part of first and last bar
    y_range <- c(max(id$lap) + 1 , 0)

    # format mm:ss annotations
    ## converter function for seconds, provde "unknown" time zone to avoid
    ## local time being applied by strptime...
    f <- function(x, time_format = "%M:%S") {
      x %>% as.character() %>%
        strptime(format = "%s", tz = "NA") %>%
        format(time_format)
    }

    anot_pos <- id %>%
      dplyr::group_by(lap) %>%
      dplyr::summarise(d = sum(duration)) %>%
      dplyr::select(d)
    anot <- anot_pos$d %>%
      purrr::map_chr(., f) %>%
      as.vector()

    p <- plotly::plot_ly(x = id$duration[id$iz == 0], y = id$lap[id$iz == 0],
                         name = "I0",
                         type = "bar", orientation = "h", hoverinfo = "text",
                         marker = list(color = l$pzc2[1],
                                       line = list(color = "rgb(248, 248, 249",
                                                   width = 1)
                                       )
                         )

    for (i in 1:max(id$iz)) {
      p <- plotly::add_trace(p, x = id$duration[id$iz == i],
                             y = id$lap[id$iz == i], name = paste0("I", i),
                             marker = list(color = l$pzc2[i + 1])
                             )
    }

    p %>% plotly::layout(barmode = "stack", showlegend = FALSE,
                         xaxis = list(showticklabels = FALSE, showgrid = FALSE,
                                      zeroline = FALSE),
                         yaxis = list(title = "", range = y_range,
                                      showticklabels = FALSE, showgrid = FALSE,
                                      zeroline = FALSE),
                         margin = list(l = 10, r = 10, t = 5, b = 5, pad = 2)) %>%

      add_annotations(xref = "x", yref = "y", x = 0, y = 1:length(anot),
                      xanchor = "left", text = anot, showarrow = FALSE,
                      font = list(size = 10)) %>%

      add_annotations(xref = "x", yref = "y", x = max(anot_pos$d),
                      y = 1:length(anot), xanchor = "right",
                      text = as.character(1:length(anot)), showarrow = FALSE,
                      font = list(size = 10)) %>%
      plotly::config(displayModeBar = FALSE, displayLogo = FALSE,
             modeBarButtonsToRemove = list("sendDataToCloud", "zoom2d",
                                           "pan2d", "select2d", "select2d",
                                           "zoomIn2d", "zoomOut2d", "toImage"))
  })

  output$distribution_zone_plot <- plotly::renderPlotly({

    # first, make one lap covering all, later to be given by zoom
    laps <- c(min(l$data$Time), max(l$data$Time))

    # get distribution
    id <- make_intesity_distribution(laps, l$data$HeartRateBpm, l$data$Time,
                                    l$zones)

    # format mm:ss annotations
    ## converter function for seconds
    f <- function(x) {
      x %>% as.character() %>%
        strptime(format = "%s", tz = "NA") %>%
        format("%H:%M:%S")
    }

    anot_pos <- id$duration
    anot <- id$duration %>%
      purrr::map_chr(., f) %>%
      as.vector()

    p <- plotly::plot_ly(x = anot_pos, y = id$iz,
                         #y = ~ y_vals,
                         type = "bar",
                         orientation = "h",
                         hoverinfo = "text",
                         marker = list(color=l$pzc2),
                         width = 200, height = 100)

    p %>% plotly::layout(showlegend = FALSE,
                         xaxis = list(showticklabels = FALSE,
                                      showgrid = FALSE,
                                      zeroline = FALSE),
                         yaxis = list(title = "",
                                      showticklabels = FALSE,
                                      showgrid = FALSE,
                                      zeroline = FALSE),
                         margin = list(l = 10, r = 10, t = 5, b = 5, pad = 2),
                         autosize = FALSE) %>%
      add_annotations(xref = "x", yref = "y", x = anot_pos, y = id$iz,
                      xanchor = "left", text = anot, showarrow = FALSE,
                      font = list(size = 10)) %>%
      add_annotations(xref = "x", yref = "y", x = 0, y = id$iz,
                      xanchor = "right", text = paste0("I", id$iz),
                      showarrow = FALSE, font = list(size = 10))
  })

  output$trackpoint_plot <- plotly::renderPlotly({

    # set y axis
    # pulse
    first_y <- list(title = "Heart rate", overlaying = "n", side = "left",
                    zeroline = FALSE, range = l$y1,
                    showgrid = FALSE)

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
    for (i in 1:(length(l$zones) - 0)) {
      p <- plotly::add_trace(p, y = rep(l$zones[i + 0], length(l$x_data)),
                             name = paste0("I", as.character(i - 1)),
                             mode = "none", type = "scatter", yaxis = "y1",
                             fillcolor = l$pzc[i + 0], fill = "tonexty",
                             hoverinfo = "text")
    }

    p <- plotly::add_trace(p, y = ~ l$data$FakeAltitudeMeters,
                           name = "Elevation", type = "scatter", mode = "none",
                           fill ="tozeroy", yaxis = "y2",
                           fillcolor = "rgba(190, 190, 190, 0.3)",
                           text = paste("Alt:", round(l$data$AltitudeMeters,
                                                      digits = 0)),
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
                        line = list(color = "rgba(0, 0 , 255, 0.7",
                                    shape = "spline",
                                    smoothig = 0),
                        text = paste("Pace:", l$data$PacePrintFormat),
                        hoverinfo = "text") %>%

      # fake time and distance to provide tooltips
      add_trace(y = ~ l$y1[1], name = "Time", mode = "lines",
                type = "scatter", yaxis = "y1",
                line = list(color = "rgba(255, 255, 255, 1)"),
                text = paste("Time:", strftime(l$data$Time,
                                               format = "%H:%M")),
                hoverinfo = "text") %>%

      add_trace(y = ~ l$y1[2], name = "Distance", mode = "lines",
                type = "scatter", yaxis = "y1",
                line = list(color = "rgba(255, 255, 255, 1)"),
                text = paste("Dist:", round(l$data$DistanceMeters/1000,
                                            digits = 2)),
                hoverinfo = "text") %>%
      plotly::layout(yaxis = first_y, yaxis2 = second_y,
                     yaxis3 = third_y, legend = list(orientation = "h")) %>%
      plotly::config(displayModeBar = "hover")
    p

  })

  hk <- renderText({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) -1 else as.integer(d$key[1])
  })
}
