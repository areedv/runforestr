#' Title
#'
#' @param laps vector of POSIX date and times with laps + 1 elements
#' @param pulse vector of pulse (bpm) obs
#' @param time vector of POSIX time obs (corresponding to pulse)
#' @param pulse_zones vector of pulse zone limits (bpm). For lower limits
#' endpoints are included, not for upper limits, _e.g._ the vector
#' c(40, 60, 80) equals intervalls [40, 60>, [60, 80>
#' @param use_empty_intensities logical defining if all pulse zones (as defined
#' in \emph{pulse_zones} are to be
#' provided in the distribution regardless what may be represented in the data
#' (as defined in \emph{pulse}). If TRUE dummy data with zero duration are
#' added for each lap (as defined in \emph{laps}) and pulse zone
#'
#' @return A (grouped) data frame of durations (in seconds) grouped by lap(s)
#' and intesity zone
#' @export
#'
#' @examples
make_intesity_distribution <- function(laps, pulse, time, pulse_zones,
                                       use_empty_intensities = FALSE) {

  if (length(laps) < 2) {
    stop("'laps' must contain at least two points in time")
  }

  if (length(pulse) != length(time)) {
    stop("Parameters 'tp_puls' and 'time' must be of equal length")
  }

  # use all zones, regardless of what may be present in data
  # dummy data must provide zero duration
  if (use_empty_intensities) {
    for (i in 1:(length(pulse_zones) - 1)) {
      for (j in 1:(length(laps) - 1)) {
        pulse <- c(rep(pulse_zones[i], 2), pulse)
        time <- c(rep(laps[j], 2), time)
      }
    }
  }

  # replace missing pulse observations by nearest value
  pulse <- replace_nas(pulse)

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
    dplyr::summarise(duration = sum(td, na.rm = TRUE))
}
