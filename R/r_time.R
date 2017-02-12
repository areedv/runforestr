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

  second_y <- list(title = "", #elevation
                   overlaying = "y",
                   side = "right",
                   zeroline = FALSE,
                   # keep at lower half of plot
                   range = c(min(data$FakeAltitudeMeters),
                             max(data$FakeAltitudeMeters) * 2),
                   showgrid = FALSE, showticklabels = FALSE)

  third_y <- list(title = "", #pace
                  overlaying = "y", side = "right",
                  zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                  range = c(10, 0))
                  #autorange = "reversed", range = c(0, 10))

  p <- plotly::plot_ly(x = ~ data$Time) %>%
    add_trace(y = ~ data$FakeAltitudeMeters, name = "Elevation",
              type = "scatter", mode = "none", fill ="tozeroy",
              yaxis = "y2", fillcolor = "rgba(190, 190, 190, 1)",
              text = paste("Alt:", data$AltitudeMeters),
              hoverinfo = "text") %>%
    add_trace(y = ~ data$HeartRateBpm,
              type = "scatter", mode = "line+markers", name = "HR",
              text = paste("HR:", data$HeartRateBpm),
              hoverinfo = "text") %>%
    add_trace(y = ~ data$Pace, name = "Pace", type = "scatter", mode = "line",
              yaxis = "y3",
              text = paste("Pace:", as.difftime(data$Pace, units = "mins")),
              hoverinfo = "text") %>%
    layout(yaxis2 = second_y, yaxis3 = third_y, legend = list(orientation = "h"))

  p

}
