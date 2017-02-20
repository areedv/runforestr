

parse_garmin_tcx_trackpoint <- function(tcx_nodeset, ns) {

  date_time_format <- "%FT%X" #2017-02-02T16:01:09.000Z

  TrackpointXPath <- "//d1:Lap/d1:Track"

  # parse only below the 'Track' node
  tp <- tcx_nodeset %>%
    xml2::xml_find_all(TrackpointXPath, ns)

  TimeXPath <- "d1:Trackpoint/d1:Time"
  TrackpointXPath <- "d1:Trackpoint"
  LatitudeDegreesXPath <- "d1:Position/d1:LatitudeDegrees"
  LongitudeDegreesXPath <- "d1:Position/d1:LongitudeDegrees"
  AltitudeMetersXPath <- "d1:AltitudeMeters"
  DistanceMetersXPath <- "d1:DistanceMeters"
  HeartRateBpmXPath <- "d1:HeartRateBpm/d1:Value"

  # assume a Time node for every record, always UTC (?)
  Time <- xml2::xml_find_all(tp, TimeXPath, ns) %>%
    xml2::xml_text() %>%
    strptime(., date_time_format, tz = "GMT") %>%
    as.POSIXct()

  # assume all other vars missing for some records
  LatitudeDegrees <- tp %>%
    xml2::xml_find_all(., TrackpointXPath, ns) %>%
    xml2::xml_find_first(LatitudeDegreesXPath) %>%
    xml2::xml_double()

  LongitudeDegrees <- tp %>%
    xml2::xml_find_all(TrackpointXPath, ns) %>%
    xml2::xml_find_first(LongitudeDegreesXPath, ns) %>%
    xml2::xml_double()

  AltitudeMeters <- tp %>%
    xml2::xml_find_all(TrackpointXPath, ns) %>%
    xml2::xml_find_first(AltitudeMetersXPath, ns) %>%
    xml2::xml_double()

  DistanceMeters <- tp %>%
    xml2::xml_find_all(TrackpointXPath, ns) %>%
    xml2::xml_find_first(DistanceMetersXPath, ns) %>%
    xml2::xml_double()

  HeartRateBpm <- tp %>%
    xml2::xml_find_all(TrackpointXPath, ns) %>%
    xml2::xml_find_first(HeartRateBpmXPath, ns) %>%
    xml2::xml_integer()

  tibble::tibble(Time = Time, LatitudeDegrees = LatitudeDegrees,
                 LongitudeDegrees = LongitudeDegrees,
                 AltitudeMeters = AltitudeMeters,
                 DistanceMeters = DistanceMeters,
                 HeartRateBpm = HeartRateBpm)

}
