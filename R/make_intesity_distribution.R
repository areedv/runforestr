#' Title
#'
#' @param laps vector of POSIX date and times with laps + 1 elements
#' @param pulse vector of pulse (bpm) obs
#' @param time vector of POSIX time obs (corresponding to pulse)
#' @param pulse_zones vector of pulse zone limits (bpm). For lower limits
#' endpoints are included, not for upper limits, _e.g._ the vector
#' c(40, 60, 80) equals intervalls [40, 60>, [60, 80>
#'
#' @return A (grouped) data frame of durations (in seconds) grouped by lap(s)
#' and intesity zone
#' @export
#'
#' @examples
make_intesity_distribution <- function(laps, pulse, time,
                                       pulse_zones) {

  if (length(laps) < 2) {
    stop("'laps' must contain at least two points in time")
  }

  if (length(pulse) != length(time)) {
    stop("Parameters 'tp_puls' and 'time' must be of equal length")
  }

  # make intensity zone for each pulse record, include endpoint in last zone
  iz <- findInterval(pulse, pulse_zones, rightmost.closed = TRUE)

  # make timediff between records, set last obs to zero duration
  n <- length(time)
  td <- as.numeric(difftime(time[2:n], time[1:(n - 1)], units = "secs"))
  td <- c(td, 0)

  # make lap id for each record,  include endpoint at end of last lap
  lap <- findInterval(time, laps, rightmost.closed = TRUE)

  # make a nice data frame to work on
  df <- data.frame(lap=lap, iz=iz, td=td)

  # group by lap and intesity zone and sum duration to make intensity
  # distribution
  id <- df %>%
    dplyr::group_by(lap, iz) %>%
    dplyr::summarise(duration = sum(td))
}
