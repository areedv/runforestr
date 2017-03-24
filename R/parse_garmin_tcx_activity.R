#' Title
#'
#' @param tcx_nodeset
#' @param ns
#'
#' @return
#' @export
#'
#' @examples

parse_garmin_tcx_activity <- function(tcx_nodeset, ns) {

  ActivityIdXPath <- "//d1:Id"
  SportXPath <- "//@Sport"
  DeviceIdXPath <- "//d1:Creator/d1:UnitId"
  DeviceNameXPath <- "//d1:Creator/d1:Name"

  date_time_format <- "%FT%X" #2017-02-02T16:01:09.000Z

  ActivityId <- tcx_nodeset %>%
    xml2::xml_find_first(ActivityIdXPath, ns) %>%
    xml2::xml_text()

  DateTime <- Id %>%
    strptime(., date_time_format, tz = "GMT") %>%
    as.POSIXct()

  Sport <- tcx_nodeset %>%
    xml2::xml_find_first(SportXPath, ns) %>%
    xml2::xml_text()

  DeviceId <- tcx_nodeset %>%
    xml2::xml_find_first(DeviceIdXPath, ns) %>%
    xml2::xml_text()

  DeviceName <- tcx_nodeset %>%
    xml2::xml_find_first(DeviceNameXPath, ns) %>%
    xml2::xml_text()

  tibble::tibble(ActivityId = ActivityId, Sport = Sport, DateTime = DateTime,
                 DeviceId = DeviceId, DeviceName = DeviceName)
}
