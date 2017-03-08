library(magrittr)
library(plotly)
library(runforestr)

function(input, output, session) {

  k <- r_map(t$data)
  k <- dplyr::transmute(k, lng = LongitudeDegrees, lat = LatitudeDegrees)

  t0 <- min(t$data$Time)
  t1 <- max(t$data$Time)

  l <- r_time(t$data, t0, t1)

  # reactive trackpoint select data
  #output$test_panel <- renderText({
  sk <- reactive({
    sk <- plotly::event_data("plotly_relayout", source = "trackpoint")
    # value 0 is at the start of the epoch which will be returned when
    # resetting plot
    if (is.null(sk)) {
      c(t0, t1)
    } else if (sk[1] == "TRUE") {
      c(t0, t1)
    } else {
      c(as.numeric(sk[1])/1000, as.numeric(sk[2])/1000)
    }
  })

  # make filtered tp data for laps and intensity distribution
  tp_filter <- reactive({
    dplyr::filter(l$data, as.numeric(Time) >= as.numeric(sk()[1]) &
                    as.numeric(Time) <= as.numeric(sk()[2]))
  })

  # reactive trackpoint hover data
  hk <- reactive({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) -1 else as.integer(d$key[1])
  })

  th <- reactive({
    k <- dplyr::mutate(k, key = as.integer(row.names(k)))
    k <- dplyr::filter(k, key == hk())
    k <- dplyr::select(k, lng, lat)
    k
  })



  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::clearBounds()
    if (!is.null(t)) {
      m <- leaflet::addPolylines(m, k$lng, k$lat)
      # shift view se
      lngs <- m$x$limits$lng
      lats <- m$x$limits$lat
      m <- leaflet::fitBounds(m, lng1 = lngs[1], lat1 = lats[1] - diff(lats),
                              lng2 = lngs[2] + diff(lngs), lat2 = lats[2])
    }
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

  output$trackpoint_plot <- plotly::renderPlotly({

    trackpoint_plot(l)

  })

  output$distribution_lap_plot <- plotly::renderPlotly({

    ll <- tp_filter()

    if (input$lap_type == "equal distance"){
      #laps <- make_laps_distance(l$data$Time, l$data$DistanceMeters,
      #                           convert_factor = 1000)
      laps <- make_laps_distance(ll$Time, ll$DistanceMeters,
                                 convert_factor = 1000)
    } else {
      #laps <- t$meta$Laps
      laps <- dplyr::distinct(ll, Lap) %>% magrittr::use_series(Lap)
    }

    id <- make_intesity_distribution(laps, ll$HeartRateBpm, ll$Time,
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

    if (is.null(t)) {
      plotly::plotly_empty()
    } else {

      ll <- tp_filter()
      # make one lap
      laps <- c(min(ll$Time), max(ll$Time))

      # get distribution
      id <- make_intesity_distribution(laps, ll$HeartRateBpm, ll$Time,
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

      # make indices for color vector corresponding to intensity zones
      # represented
      color_low <- min(id$iz) + 1
      color_high <- max(id$iz) + 1

      p <- plotly::plot_ly(x = anot_pos, y = id$iz,
                           #y = ~ y_vals,
                           type = "bar",
                           orientation = "h",
                           hoverinfo = "text",
                           marker = list(color=l$pzc2[color_low:color_high]),
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
    }
  })
}
