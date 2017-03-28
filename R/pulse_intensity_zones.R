#' Provide absolute pulse intensity zones
#'
#' Based on individual pulse data, realative intesity zones and model
#' specification this function provide individual pulse intensity zones
#'
#' @param pulseRest integer heart rate at rest
#' @param pulseMax integer maximum heart rate
#' @param relativeIntesities integer vector of intensity zone intervals
#'   including endpoints
#' @param model string model name used for calculating intensity zones, either
#'   \emph{max_pulse} [default] or \emph{heart_rate_reserve}
#'
#' @return integer vector with intesity heart rate intervals including
#'   endpoints
#' @export
#'
#' @examples

pulse_intesity_zones <- function(pulseRest, pulseMax, relativeIntesities,
                                 model = "maxPulse") {

  if (model == "maxPulse") {
    zones <- round(relativeIntesities * pulseMax, digits = 0)
  } else if (model == "heartRateReserve") {
    reserve <- pulseMax - pulseRest
    zones <- round(relativeIntesities * reserve + pulseRest, digits = 0)
  } else {
    stop("The model provided is none of 'max_pulse' or 'heart_rate_reserve'")
  }

  zones
}
