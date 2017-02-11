#' Title
#'
#' @param meta
#' @param data
#'
#' @return
#' @export
#'
#' @examples
postprocess_track <- function(meta, data) {

  # link laps to each trackpoint
  laps <- dim(meta)[1]
  lap <- rep(Sys.time(), dim(data)[1])

  for (i in 1:laps-1) {
    ind <- data$Time >= meta$Laps[i] & data$Time < meta$Laps[i+1]
    lap[ind] <- meta$Laps[i]
  }

  # set last lap link in data
  ind <- data$Time >= meta$Laps[laps]
  lap[ind] <- meta$Laps[laps]
  data <- tibble::add_column(data, Lap = lap)

  # add pace
  len <- length(data$Time)
  delta_mins <- difftime(data$Time[2:len], data$Time[1:len-1], units = "mins")
  delta_km <- (data$DistanceMeters[2:len]-data$DistanceMeters[1:len-1]) / 1000
  Pace <- c(NA, delta_mins / delta_km)
  data <- tibble::add_column(data, Pace = Pace)

}
