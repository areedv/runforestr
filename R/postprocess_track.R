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

  # possible to change timezone here?
  attr(meta$Laps, "tzone") <- "CET"
  attr(data$Time, "tzone") <- "CET"

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

  # add pace, add first element to retain length and filter by limit minimums
  # as defined in config
  len <- length(data$Time)
  delta_mins <- difftime(data$Time[2:len], data$Time[1:len-1], units = "mins")
  delta_dist <- (data$DistanceMeters[2:len]-data$DistanceMeters[1:len-1]) /
    conf$obs$conversion$distance_denominator
  Pace <- c(conf$runner$paceMin, delta_mins / delta_dist)
  Pace[Pace > conf$runner$paceMin] <- conf$runner$paceMin
  data <- tibble::add_column(data, Pace = Pace)

  # add pace, nicely formatted as %M%S (mm:ss), via the epoch
  ppf <- as.character(data$Pace * 60) %>%
    strptime(format = "%s") %>%
    format("%M:%S")

  data <- tibble::add_column(data, PacePrintFormat = ppf)

  # add speed, add first element (of zero velocity) to retain length
  # also extra column for nice printing
  delta_hours <- as.numeric(delta_mins) / 60
  Speed <- c(0, delta_dist / delta_hours)
  data <- tibble::add_column(data, Speed = Speed,
                             SpeedPrintFormat = round(Speed, digits = 1))

}
