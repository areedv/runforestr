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

  # for nicer plot, smooth and force zero altitude below sealevel
  alt <- runmed(data$AltitudeMeters, 11)
  alt[alt <= 0] <- 0
  data <- dplyr::mutate(data, AltitudeMeters = alt)


  second_y <- list(title = "Elevation",
                   overlaying = "y",
                   side = "right",
                   zeroline = FALSE,
                   rangemode = "tozero",
                   showgrid = FALSE)

  p <- plotly::plot_ly(x = ~ data$Time) %>%
    add_trace(y = ~ data$AltitudeMeters, name = "Elevation",
              type = "scatter", mode = "none", fill ="tozeroy",
              yaxis = "y2", fillcolor = "rgba(190, 190, 190, 0.3)",
              alpha = 1) %>%
    add_trace(y = ~ data$HeartRateBpm,
              type = "scatter", mode = "line+markers", name = "HR",
              text = paste("HR:", data$HeartRateBpm)) %>%
    layout(yaxis2 = second_y, legend = list(orientation = "h"))

  p

}
