#' Title
#'
#' @param laps
#' @param tp_pulse
#' @param tp_time
#' @param pulse_zones
#'
#' @return
#' @export
#'
#' @examples
make_intesity_distribution <- function(laps, tp_pulse, tp_time,
                                       pulse_zones) {

  if (length(laps) < 2) {
    stop("'laps' must contain at least two points in time")
  }

  if (length(tp_pulse) != length(tp_time)) {
    stop("Parameters 'tp_puls' and 'tp_time must be of equal length")
  }

  # make empty df for intensity distribution
  n <- length(pulse_zones) - 1
  idist <- as.data.frame(matrix(ncol = n))
  ireldist <- idist

  # a tibble of data including timediff, last obs set to zero duration
  n <- length(tp_pulse)
  d <- as.numeric(difftime(tp_time[2:n], tp_time[1:(n - 1)], units = "secs"))
  data <- tibble::tibble(pulse=tp_pulse, time=tp_time,
                        duration = c(d, 0))

  # iterate over laps
  for (i in 1:(length(laps) - 1)) {

    # this lap
    lap <- dplyr::filter(data, time >= laps[i] & time < laps[i + 1])

    # iterate zones
    it <- vector()
    for (j in 1:(length(pulse_zones) - 1)) {
      ind <- lap$pulse >= pulse_zones[j] & lap$pulse < pulse_zones[j + 1]
      it[j] <- sum(lap$duration[ind])
    }
    idist <- rbind(idist, it)
    ireldist <- rbind(ireldist, (it / sum(it)))
  }

  # remove the first empty (NA) entry from creation...
  n <- dim(idist)[1]
  idist <- idist[2:n,]

  list(idist = tibble::as_tibble(idist), ireldist = tibble::as_tibble(ireldist))
}
