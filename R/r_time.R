#' Title
#'
#' @param data
#' @param t0
#' @param t1
#' @param segments
#'
#' @return
#' @export
#'
#' @examples
r_time <- function(data, t0, t1, segments) {

  # for nicer plot:
  # smooth and fake altitude if obs below sealevel
  alt <- runmed(data$AltitudeMeters, 11)
  data <- dplyr::mutate(data, AltitudeMeters = alt)
  minAlt <- min(data$AltitudeMeters)
  if ( minAlt < 0) {
    alt <- alt - minAlt
  }
  data <- dplyr::mutate(data, FakeAltitudeMeters = alt)


  # smooth Pace
  pace <- c(NA, runmed(na.omit(data$Pace), 3))
  data <- dplyr::mutate(data, Pace = pace)

  # get config
  if (!exists("conf")) {
    conf <- yaml::yaml.load_file(system.file("rfr.yml", package = "runforestr"))
  }

  # pulse
  first_y <- list(title = "Heart rate", overlaying = "n", side = "left",
                  zeroline = FALSE,
                  #range = c(min(data$HeartRateBpm), max(data$HeartRateBpm)),
                  range = c(conf$runner$pulseRest, conf$runner$pulseMax),
                  showgrid = FALSE)

  # elevation
  second_y <- list(title = "",
                   overlaying = "y",
                   side = "right",
                   zeroline = FALSE,
                   # keep at lower half of plot
                   range = c(min(data$FakeAltitudeMeters),
                             max(data$FakeAltitudeMeters) * 2),
                   showgrid = FALSE, showticklabels = FALSE)
  # pace
  third_y <- list(title = "",
                  overlaying = "y", side = "right",
                  zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                  range = c(conf$runner$paceMin, conf$runner$paceMax))

  # x-axis should be selected from function parameter, static for now
  x_data <- data$Time

  # set puls zone colors and alpha in five levels starting on max intesity
  pzc <- c("rgba(255, 100, 100, 0.3)", "rgba(255, 255, 100, 0.3)",
           "rgba(100, 255, 100, 0.3)", "rgba(100, 100, 255, 0.3)",
           "rgba(200, 200, 255, 0.3)", "rgba(255, 255, 255, 0.3)")

  # recycle the last (zone 1, white) for below zones
  zones <- pulse_intesity_zones(pulseRest = conf$runner$pulseRest,
                                pulseMax = conf$runner$pulseMax,
                                relativeIntesities = conf$runner$intensityZones,
                                model = conf$runner$intesityZoneModel)

  redundant_zones <- length(zones) - length(pzc)
  if (redundant_zones > 0) {
    pzc <- c(pzc, rep(pzc[length(pzc)], redundant_zones))
  }
  pzc <- rev(pzc)
  #zones <- rev(zones)

  p <- plotly::plot_ly(x = ~ x_data, mode = "lines")
  # test with pulse zones
  for (i in 1:(length(zones) - 1)) {
    p <- plotly::add_trace(p, y = rep(zones[i + 1], length(x_data)),
                           name = paste0("I", as.character(i)),
                           mode = "none", type = "scatter", yaxis = "y1",
                           fillcolor = pzc[i + 1], fill = "tonexty",
                           hoverinfo = "text")
  }

  p <- plotly::add_trace(p, y = ~ data$FakeAltitudeMeters, name = "Elevation",
                         type = "scatter", mode = "none", fill ="tozeroy",
                         yaxis = "y2", fillcolor = "rgba(190, 190, 190, 0.3)",
                         text = paste("Alt:", data$AltitudeMeters),
                         hoverinfo = "text") %>%
    plotly::add_trace(y = ~ data$HeartRateBpm,
                      type = "scatter", mode = "none", name = "HR",
                      line = list(color = "rgb(0, 0, 0)", dash = "dot"),
                      yaxis = "y1", fill = "tozeroy", fillcolor = "rgba(255, 255, 255, 1)",
                      text = paste("HR:", data$HeartRateBpm),
                      hoverinfo = "text") %>%

    plotly::add_trace(y = ~ data$Pace, name = "Pace", type = "scatter", mode = "line",
                      yaxis = "y3",
                      text = paste("Pace:", as.difftime(data$Pace, units = "mins")),
                      hoverinfo = "text") %>%
    plotly::layout(yaxis = first_y, yaxis2 = second_y, yaxis3 = third_y,
                   legend = list(orientation = "h"))

  p

}
