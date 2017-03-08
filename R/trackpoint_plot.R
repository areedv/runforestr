#' Title
#'
#' @param l data needed for making gplot
#'
#' @return A plotly object
#' @export
#'
#' @examples

trackpoint_plot <- function(l) {

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
                   yaxis3 = third_y, legend = list(orientation = "h"),
                   dragmode = "select") %>%
    plotly::config(displayModeBar = "hover")
  p

}
