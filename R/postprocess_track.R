postprocess_track <- function(meta, data) {

  # link laps to each trackpoint
  laps <- dim(meta)[1]
  lap <- rep(NA, dim(data)[1])

  for (i in 1:laps-1) {
    ind <- data$Time >= meta$Laps[i] & data$Time < meta$Laps[i+1]
    lap[ind] <- meta$Laps[i]
  }
  # set last lap link in data
  ind <- data$Time >= meta$Laps[laps]
  lap[ind] <- meta$Laps[laps]

  data <- tibble::add_column(data, Lap = lap)

}
