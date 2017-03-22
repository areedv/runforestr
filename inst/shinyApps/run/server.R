library(magrittr)
library(plotly)
library(runforestr)

function(input, output, session) {

  # chain of data reactives
  # main data as loaded from tcx, other sources such as stored data should be
  # handled here too
  dat <- reactive({
    req(input$selected_data)
    parse_garmin_tcx(input$selected_data$datapath)
  })

  # trackpoint data depend on dat
  tpdat <- reactive({
    d <- dat()
    t0 <- min(d$data$Time)
    t1 <- max(d$data$Time)
    r_time(d$data, t0, t1)
  })

  # map data depend on dat
  mapdat <- reactive({
    d <- r_map(dat()$data) %>%
      dplyr::transmute(lng = LongitudeDegrees, lat = LatitudeDegrees)
    # empty position data?
    if (length(d$lng) == length(d$lng[is.na(d$lng)]) |
        length(d$lat) == length(d$lat[is.na(d$lat)])) {
      return(FALSE)
    } else {
      return(d)
    }
  })

  # reactive trackpoint select data, depend on event data from trackpoint plot
  # and dat. Time is returned as milliseconds by plotly
  tp_selectdat <- reactive({
    # we need timeframe
    d <- dat()
    t0 <- min(d$data$Time)
    t1 <- max(d$data$Time)
    # event data from plotly
    d <- plotly::event_data("plotly_relayout", source = "trackpoint")
    # value 0 is at the start of the epoch which will be returned when
    # resetting plot
    if (is.null(d)) {
      c(t0, t1)
    } else if (d[1] == "TRUE") {
      c(t0, t1)
    } else {
      c(as.numeric(d[1])/1000, as.numeric(d[2])/1000)
    }
  })

  # make filtered tp data for laps and intensity distribution, depend on dat
  # and tp_selectdat
  tp_filter <- reactive({
    dplyr::filter(dat()$data, as.numeric(Time) >= as.numeric(tp_selectdat()[1]) &
                    as.numeric(Time) <= as.numeric(tp_selectdat()[2]))
  })

  # reactive trackpoint hover data, depend on event data from trackpoint plot
  tp_hoverdat <- reactive({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) -1 else as.integer(d$key[1])
  })

  th <- reactive({
    d <- mapdat()
    if (is.logical(d)) {
      return(NULL)
    }
    d <- dplyr::mutate(d, key = as.integer(row.names(d)))
    d <- dplyr::filter(d, key == tp_hoverdat())
    d <- dplyr::select(d, lng, lat)
    d
  })

  # end chain of reactive data


  # observers
  observe({
    d <- mapdat()
    if (!is.logical(d)) {
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
    }
  })

  # outputs
  output$trackpoint_plot <- plotly::renderPlotly({
    trackpoint_plot(tpdat())
  })

  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::clearBounds()
    # map only if position data
    dat <- mapdat()
    if (!is.logical(dat)) {
      m <- leaflet::addPolylines(m, dat$lng, dat$lat)
      # shift view se
      lngs <- m$x$limits$lng
      lats <- m$x$limits$lat
      m <- leaflet::fitBounds(m, lng1 = lngs[1], lat1 = lats[1] - diff(lats),
                              lng2 = lngs[2] + diff(lngs), lat2 = lats[2])
    }
    m
  })

  # observeEvent(input$selected_data, {
  #   inFile <- input$selected_data
  #   #if (is.null(inFile)) {
  #   #  return(NULL)
  #   #  print("no file")
  #   #}
  #   print(paste("Selected file:", inFile$datapath, inFile$name))
  #   dat <- parse_garmin_tcx(inFile$datapath)
  #   print("do we have dat?")
  # })
  #
  #
  # dat <- eventReactive(input$selected_data, {
  #   print("parsing data")
  #   inFile <- input$selected_data
  #   parse_garmin_tcx(inFile$datapath)
  #   #print("Calling module 'interactiveMap'")
  #   #callModule(interactiveMap, "interactiveMap", t=dat())
  # })
  #
  # # if (!is.null(dat())) {
  # #   print("data is populated")
  # # }
  #
  #


  # observe({
  #   print("observing...")
  #   print("Calling module 'interactiveMap'")
  #   callModule(interactiveMap, "interactiveMap", t=dat())
  #   print("Module 'interactiveMap' was called")
  # })

  # output$renderInteractiveMap <- renderUI({
  #   print("Do render UI?")
  #   callModule(interactiveMap, "interactiveMap", t=dat())
  #   interactiveMapInput("interactiveMap", dat())
  # })


  # k <- r_map(t$data)
  # k <- dplyr::transmute(k, lng = LongitudeDegrees, lat = LatitudeDegrees)
  #
  # # empty position data?
  # if (length(k$lng) == length(k$lng[is.na(k$lng)]) |
  #     length(k$lat) == length(k$lat[is.na(k$lat)])) {
  #   contain_pos <- FALSE
  # } else {
  #   contain_pos <- TRUE
  # }
  #
  # t0 <- min(t$data$Time)
  # t1 <- max(t$data$Time)
  #
  # l <- r_time(t$data, t0, t1)
  #
  # # reactive trackpoint select data
  # #output$test_panel <- renderText({
  # sk <- reactive({
  #   sk <- plotly::event_data("plotly_relayout", source = "trackpoint")
  #   # value 0 is at the start of the epoch which will be returned when
  #   # resetting plot
  #   if (is.null(sk)) {
  #     c(t0, t1)
  #   } else if (sk[1] == "TRUE") {
  #     c(t0, t1)
  #   } else {
  #     c(as.numeric(sk[1])/1000, as.numeric(sk[2])/1000)
  #   }
  # })
  #
  # # make filtered tp data for laps and intensity distribution
  # tp_filter <- reactive({
  #   dplyr::filter(l$data, as.numeric(Time) >= as.numeric(sk()[1]) &
  #                   as.numeric(Time) <= as.numeric(sk()[2]))
  # })
  #
  # # reactive trackpoint hover data
  # hk <- reactive({
  #   d <- plotly::event_data("plotly_hover", source = "trackpoint")
  #   if (is.null(d)) -1 else as.integer(d$key[1])
  # })
  #
  # th <- reactive({
  #   k <- dplyr::mutate(k, key = as.integer(row.names(k)))
  #   k <- dplyr::filter(k, key == hk())
  #   k <- dplyr::select(k, lng, lat)
  #   k
  # })
  #
  # output$trackpoint_plot <- plotly::renderPlotly({
  #
  #   trackpoint_plot(l)
  #
  # })
  #
  # output$map <- leaflet::renderLeaflet({
  #   m <- leaflet::leaflet() %>%
  #     leaflet::addProviderTiles("CartoDB.Positron") %>%
  #     leaflet::clearBounds()
  #   # map only if position data
  #   if (contain_pos) {
  #     m <- leaflet::addPolylines(m, k$lng, k$lat)
  #     # shift view se
  #     lngs <- m$x$limits$lng
  #     lats <- m$x$limits$lat
  #     m <- leaflet::fitBounds(m, lng1 = lngs[1], lat1 = lats[1] - diff(lats),
  #                             lng2 = lngs[2] + diff(lngs), lat2 = lats[2])
  #   }
  #   m
  # })
  #
  # observe({
  #   if (contain_pos) {
  #     marker <- th()
  #     # clear all markers when empty hover data
  #     if (dim(marker)[1] == 0) {
  #       leaflet::leafletProxy("map", data = marker) %>%
  #         leaflet::clearGroup("markers")
  #     } else {
  #       leaflet::leafletProxy("map", data = marker) %>%
  #         leaflet::addCircleMarkers(radius = 7, weight = 1, color = "#777777",
  #                                   fillColor = "#999999", fillOpacity = 0.3,
  #                                   group = "markers")
  #     }
  #   }
  # })
  #
  output$distribution_lap_plot <- plotly::renderPlotly({

    d <- tp_filter()
    l <- tpdat()

    if (input$lap_type == "equal distance"){
      laps <- make_laps_distance(d$Time, d$DistanceMeters,
                                 convert_factor = 1000)
    } else {
      laps <- dplyr::distinct(d, Lap) %>% magrittr::use_series(Lap)
      if (length(laps) <= 1) {
        laps <- c(min(d$Time), max(d$Time))
      }
    }

    id <- make_intesity_distribution(laps, d$HeartRateBpm, d$Time,
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
                      font = list(size = 10)) #%>%
      # plotly::config(displayModeBar = FALSE, displayLogo = FALSE,
      #                modeBarButtonsToRemove = list("sendDataToCloud", "zoom2d",
      #                                              "pan2d", "select2d", "select2d",
      #                                              "zoomIn2d", "zoomOut2d", "toImage"))

  })

  output$distribution_zone_plot <- plotly::renderPlotly({

    # if (is.null(t)) {
    #   plotly::plotly_empty()
    # } else {

      d <- tp_filter()
      l <- tpdat()

      # make one lap
      laps <- c(min(d$Time), max(d$Time))

      # get distribution
      id <- make_intesity_distribution(laps, d$HeartRateBpm, d$Time,
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
    # }
  })
}
