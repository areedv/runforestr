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

  # prepare for reactive data
  if(missing(t0)) {
    t0 <- min(data$Time)
  }

  if (missing(t1)) {
    t1 <- max(data$Time)
  }

  # filter by time window
  data <- dplyr::filter(.data = data, as.numeric(Time) >= as.numeric(t0) &
                          as.numeric(Time) <= as.numeric(t1))

  # for nicer plot:
  # replace NA (if any) by using closest value,
  # smooth and fake altitude if obs below sealevel
  alt <- data$AltitudeMeters %>%
    replace_nas() %>% runmed(11)
  data <- dplyr::mutate(data, AltitudeMeters = alt)
  minAlt <- min(na.omit(data$AltitudeMeters))
  if ( minAlt < 0) {
    alt <- alt - minAlt
  }
  data <- dplyr::mutate(data, FakeAltitudeMeters = alt)


  # smooth Pace, fake values from closest element replacing NAs
  #pace <- c(NA, runmed(na.omit(data$Pace), 3))
  pace <- data$Pace %>%
    replace_nas() %>% runmed(3)
  data <- dplyr::mutate(data, Pace = pace)

  # get config
  if (!exists("conf")) {
    conf <- yaml::yaml.load_file(system.file("rfr.yml", package = "runforestr"))
  }

  # range of y axis
  ## pulse
  y1 <- c(conf$runner$pulseRest, conf$runner$pulseMax)

  # elevation
  y2 <- c(min(data$FakeAltitudeMeters), max(data$FakeAltitudeMeters) * 2)

  # pace
  y3 <- c(conf$runner$paceMin, conf$runner$paceMax * 1.1)


  # plotly does not understand time zones, convert here
  # attr(data$Time, "tzone") <- "CET"

  # x-axis should be selected from function parameter, static for now
  x_data <- data$Time

  # set puls zone colors and alpha in five levels starting on max intesity
  pzc <- c("rgba(255, 100, 100, 0.3)", "rgba(255, 255, 100, 0.3)",
           "rgba(100, 255, 100, 0.3)", "rgba(100, 100, 255, 0.3)",
           "rgba(200, 200, 255, 0.3)", "rgba(255, 255, 255, 0.3)")
  pzc2 <- c("rgba(255, 100, 100, 0.5)", "rgba(255, 255, 100, 0.5)",
            "rgba(100, 255, 100, 0.5)", "rgba(100, 100, 255, 0.5)",
            "rgba(200, 200, 255, 0.5)", "rgba(255, 255, 255, 0.5)")

  # recycle the last (zone 1, white) for below zones
  zones <- pulse_intesity_zones(pulseRest = conf$runner$pulseRest,
                                pulseMax = conf$runner$pulseMax,
                                relativeIntesities = conf$runner$intensityZones,
                                model = conf$runner$intesityZoneModel)
  redundant_zones <- length(zones) - length(pzc)
  if (redundant_zones > 0) {
    pzc <- c(pzc, rep(pzc[length(pzc)], redundant_zones))
  }
  pzc <- rev(pzc)
  pzc2 <- rev(pzc2)


  list(data=data, y1=y1, y2=y2, y3=y3, x_data=x_data, pzc=pzc,
       pzc2=pzc2, zones=zones)

}
