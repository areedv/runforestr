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
  # smooth and force zero altitude below sealevel
  alt <- runmed(data$AltitudeMeters, 11)
  alt[alt <= 0] <- 0
  data <- dplyr::mutate(data, AltitudeMeters = alt)

  # smooth Pace
  pace <- c(NA, runmed(na.omit(data$Pace), 3))
  data <- dplyr::mutate(data, Pace = pace)

  second_y <- list(title = "", #elevation
                   overlaying = "y",
                   side = "right",
                   zeroline = FALSE,
                   rangemode = "tozero",
                   showgrid = FALSE, showticklabels = FALSE)

  third_y <- list(title = "", #pace
                  overlaying = "y", side = "right",
                  zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                  range = c(10, 0))
                  #autorange = "reversed", range = c(0, 10))

  p <- plotly::plot_ly(x = ~ data$Time) %>%
    add_trace(y = ~ data$AltitudeMeters, name = "Elevation",
              type = "scatter", mode = "none", fill ="tozeroy",
              yaxis = "y2", fillcolor = "rgba(190, 190, 190, 0.3)",
              alpha = 1) %>%
    add_trace(y = ~ data$HeartRateBpm,
              type = "scatter", mode = "line+markers", name = "HR",
              text = paste("HR:", data$HeartRateBpm)) %>%
    add_trace(y = ~ data$Pace, name = "Pace", type = "scatter", mode = "line",
              yaxis = "y3",
              text = paste("Pace:", as.difftime(data$Pace, units = "mins"))) %>%
    layout(yaxis2 = second_y, yaxis3 = third_y, legend = list(orientation = "h"))

  p

}
