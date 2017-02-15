#' Make laps of equal distance
#'
#' Make laps timestamps based on integer break points in distance
#'
#' @param tp_time POSIXct vector of trackpoint timestamps
#' @param tp_cum_distance number vector of cumulative distance of same length
#' as \emph{tp_time}
#' @param convert_factor number any conversion factor for distance. Default
#' value set to 1000 assuming distance in meters and laps to be given each km
#'
#' @return POSIXct lap timestamps including input data endpoints
#' @export
#'
#' @examples

make_laps_distance <- function(tp_time, tp_cum_distance,
                               convert_factor = 1000) {

  if (length(tp_time) != length(tp_cum_distance)) {
    stop("Time and distance arguments must be of same length")
  }

  # convert cumulative distance to whatever specified
  d <- tp_cum_distance / convert_factor

  # find indices of integer break points
  l <- length(d)
  integer_delta <- floor(d[2:l]) - floor(d[1:l - 1])
  ind <- integer_delta == 1
  # include first element as break point (start of first, and end of last lap)
  ind <- c(TRUE, ind[1:length(ind) - 1], TRUE)

  # make laps
  laps <- tp_time[ind]
}
