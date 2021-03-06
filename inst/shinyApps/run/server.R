library(magrittr)
library(plotly)
library(runforestr)

# get config and assign to global environment
if (!exists("conf")) {
  assign("conf",
         yaml::yaml.load_file(system.file("rfr.yml", package = "runforestr")),
         envir = .GlobalEnv)
  }

# load data if defined
if (conf$store$use) {
  data <- load_activity()
}

shinyServer(function(input, output, session) {

  # when import requested
  import <- reactive({
    req(input$import_data)
    d <- parse_garmin_tcx(input$import_data$datapath)
    save_activity(d)
  })

  # chain of data reactives

  # all data in store, if any
  alldat <- reactive({
    if (conf$store$use) {
      load_activity()
    } else {
      req(FALSE)
    }
  })

  # limit all data by date
  date_range_dat <- reactive({
    req(input$date_range)
    get_timerange(data = alldat(), range = input$date_range)
  })

  # main data as loaded from file or selected from store
  dat <- reactive({
    req(input$select_data)
    get_activity(data = date_range_dat(), id = input$select_data)
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
  tp_filterdat <- reactive({
    d <- tp_selectdat()
    dplyr::filter(dat()$data, as.numeric(Time) >= as.numeric(d[1]) &
                    as.numeric(Time) <= as.numeric(d[2]))
  })

  # reactive trackpoint hover data, depend on event data from trackpoint plot
  tp_hoverdat <- reactive({
    d <- plotly::event_data("plotly_hover", source = "trackpoint")
    if (is.null(d)) 0 else as.integer(d$key[1])
  })

  # reactive filter and select for map markers, depend on mapdat and
  # tp_hoverdat
  tp_markerdat <- reactive({
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
      marker <- tp_markerdat()
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

  observe(import())



  # outputs

  # ui for selecting time frame
  output$dateRange <- renderUI({
    d <- alldat()
    dateRangeInput(inputId = "date_range", label = "Date range",
                   start = min(d$acti$DateTime), end = max(d$acti$DateTime))
  })

  # ui for selecting data
  output$selectData <- renderUI({
    d <- date_range_dat()
    selectInput(inputId = "select_data",
                label = "Select data",
                choices = setNames(d$acti$ActivityId,
                                   paste(format(d$acti$DateTime,
                                          "%a %d. %b %Y"), d$acti$Sport)),
                multiple = TRUE)
  })

  # ui for summary table
  output$tbl = DT::renderDataTable({

    # format a given number of mins
    f <- function(decimal_mins, print_format = "%M:%S") {
      as.character(decimal_mins * 60) %>%
        strptime(format = "%s", tz="Greenwich") %>%
        format(print_format)
    }

    s <- summary_table(date_range_dat())
    # t <- dplyr::tibble(Type=s$Sport, Time=strftime(s$DateTime, format="%F %R"),
    #                    duration = f(s$dur, "%H:%M"),
    #                    km=round(s$km, digits = 1),
    #                    maxHR=s$maxHR,
    #                    mSpeed=round(s$mspeed, digits = 1),
    #                    mPace=f(s$mpace),
    #                    year = as.numeric(strftime(s$DateTime, "%Y")),
    #                    Month = as.numeric(strftime(s$DateTime, "%m")),
    #                    Week = as.numeric(strftime(s$DateTime, "%U")),
    #                    Day = as.numeric(strftime(s$DateTime, "%u"))
    # ) %>% arrange(desc(Time))

    # which grouping?
    g <- "month" #"week"
    if (input$dataPeriod == "month") {
      s <- s %>%
        dplyr::mutate(group=paste0(as.character(lubridate::year(DateTime)),
                                   as.character(lubridate::month(DateTime))),
                      Period=lubridate::month(DateTime, label = TRUE))
    }
    if (input$dataPeriod == "week") {
      s <- s %>%
        dplyr::mutate(group=paste0(as.character(lubridate::year(DateTime)),
                                   as.character(lubridate::week(DateTime))),
                      Period=lubridate::week(DateTime))
    }
    t <- dplyr::group_by(s, group) %>%
      summarise(Year=min(lubridate::year(DateTime)),
                Period=min(Period),
                Distance=round(sum(km), digits = 1),
                Duration=f(sum(dur), "%Hh %Mm"), N=n())
    t
    })

  output$trackpoint_plot <- plotly::renderPlotly({
    trackpoint_plot(tpdat())
  })

  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::clearBounds()
    # map only if position data
    d <- mapdat()
    if (!is.logical(d)) {
      m <- leaflet::addPolylines(m, d$lng, d$lat)
      # shift view se
      lngs <- m$x$limits$lng
      lats <- m$x$limits$lat
      m <- leaflet::fitBounds(m, lng1 = lngs[1], lat1 = lats[1] - diff(lats),
                              lng2 = lngs[2] + diff(lngs), lat2 = lats[2])
    }
    m
  })

  output$distribution_lap_plot <- plotly::renderPlotly({

    d <- tp_filterdat()
    l <- tpdat()

    if (input$lap_type == "equal distance"){
      laps <- make_laps_distance(d$Time, d$DistanceMeters)
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

    p %>%
      plotly::layout(
        barmode = "stack", showlegend = FALSE,
        xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "", range = y_range, showticklabels = FALSE,
                     showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 10, r = 10, t = 5, b = 5, pad = 2)
      ) %>%
      add_annotations(
        xref = "x", yref = "y", x = 0, y = 1:length(anot), xanchor = "left",
        text = anot, showarrow = FALSE, font = list(size = 10)
      ) %>%
      add_annotations(
        xref = "x", yref = "y", x = max(anot_pos$d), y = 1:length(anot),
        xanchor = "right", text = as.character(1:length(anot)),
        showarrow = FALSE, font = list(size = 10)
      ) %>%
      plotly::config(
        displayModeBar = FALSE, displayLogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud", "zoom2d", "pan2d",
                                      "select2d", "select2d", "zoomIn2d",
                                      "zoomOut2d", "toImage")
      )
  })

  output$distribution_zone_plot <- plotly::renderPlotly({

    d <- tp_filterdat()
    l <- tpdat()

    # make one lap
    laps <- c(min(d$Time), max(d$Time))

    # get distribution
    id <- make_intesity_distribution(laps = laps, pulse = d$HeartRateBpm,
                                     time = d$Time, pulse_zones = l$zones,
                                     use_empty_intensities = TRUE)


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

    p <- plotly::plot_ly(
      x = anot_pos,
      y = id$iz,
      type = "bar",
      orientation = "h",
      hoverinfo = "text",
      marker = list(color=l$pzc2[color_low:color_high]),
      width = 200, height = 100)

    p %>% plotly::layout(
      showlegend = FALSE,
      xaxis = list(showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE),
      yaxis = list(title = "",
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE),
      margin = list(l = 10, r = 10, t = 5, b = 5, pad = 2),
      autosize = FALSE
      ) %>%
      add_annotations(
        xref = "x", yref = "y",
        x = anot_pos,
        y = id$iz,
        xanchor = "left",
        text = anot,
        showarrow = FALSE,
        font = list(size = 10)
      ) %>%
      add_annotations(
        xref = "x", yref = "y",
        x = 0,
        y = id$iz,
        xanchor = "right",
        text = paste0("I", id$iz),
        showarrow = FALSE,
        font = list(size = 10)
      ) %>%
      plotly::config(
        displayModeBar = FALSE, displayLogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud", "zoom2d", "pan2d",
                                      "select2d", "select2d", "zoomIn2d",
                                      "zoomOut2d", "toImage"))
  })
})
