#' Parse metadata from Garmin tcx file
#'
#' Pull training metadata from Garmins TrainingDatabase xml file. These are
#' summarized data for each lap such as distance, average heart rate, calories,
#' duration in addition to start time and date, intensity and (lap) trigger
#' method.
#'
#' Since input data may contain several activities, this function should not
#' be called directly but rather via parse_garmin_tcx 'ref'
#'
#' @param tcx_nodeset xml_nodeset from the tcx file
#'
#' @return a tibble with observations as rows and varables as columns
#' @export
#'
#' @examples

parse_garmin_tcx_metadata <- function(tcx_nodeset, ns) {

  date_time_format <- "%FT%X" #2017-02-02T16:01:09.000Z

  ActivityXPath <- "//d1:Activity"
  IdXPath <- "//d1:Id"
  LapStartTimeXPath <- "//d1:Lap/@StartTime"
  TotalTimeSecondsXPath <- "//d1:Lap/d1:TotalTimeSeconds"
  DistanceMetersXPath <- "//d1:Lap/d1:DistanceMeters"
  MaximumSpeedXPath <- "//d1:Lap/d1:MaximumSpeed"
  CaloriesXPath <- "//d1:Lap/d1:Calories"
  AverageHeartRateBpmXPath <- "//d1:Lap/d1:AverageHeartRateBpm/d1:Value"
  MaximumHeartRateBpmXPath <- "//d1:Lap/d1:MaximumHeartRateBpm/d1:Value"
  IntensityXPath <- "//d1:Lap/d1:Intensity"
  TriggerMethodXPath <- "//d1:Lap/d1:TriggerMethod"

  #ns <- xml2::xml_ns(tcx_nodeset)

  # use hash of Activity Id as unique identifier
  ActivityId <- tcx_nodeset %>%
    #xml2::xml_find_all(ActivityXPath, ns) %>%
    xml2::xml_find_first(IdXPath, ns) %>%
    xml2::xml_text() %>%
    digest(algo = "md5", serialize = FALSE)

  # identify activity, assume start time
  Laps <- tcx_nodeset %>%
    xml2::xml_find_all(LapStartTimeXPath, ns) %>%
    xml2::xml_text() %>%
    strptime(., date_time_format) %>%
    as.POSIXct()

  TotalTimeSeconds <- tcx_nodeset %>%
    xml2::xml_find_all(TotalTimeSecondsXPath, ns) %>%
    xml2::xml_double()

  DistanceMeters <- tcx_nodeset %>%
    xml2::xml_find_all(DistanceMetersXPath, ns) %>%
    xml2::xml_double()

  MaximumSpeed <- tcx_nodeset %>%
    xml2::xml_find_all(MaximumSpeedXPath, ns) %>%
    xml2::xml_double()

  Calories <- tcx_nodeset %>%
    xml2::xml_find_all(CaloriesXPath, ns) %>%
    xml2::xml_integer()

  AverageHeartRateBpm <- tcx_nodeset %>%
    xml2::xml_find_all(AverageHeartRateBpmXPath, ns) %>%
    xml2::xml_integer()

  MaximumHeartRateBpm <- tcx_nodeset %>%
    xml2::xml_find_all(MaximumHeartRateBpmXPath, ns) %>%
    xml2::xml_integer()

  Intensity <- tcx_nodeset %>%
    xml2::xml_find_all(IntensityXPath, ns) %>%
    xml2::xml_text()

  TriggerMethod <- tcx_nodeset %>%
    xml2::xml_find_all(TriggerMethodXPath, ns) %>%
    xml2::xml_text()

  tibble::tibble(Laps = Laps, TotalTimeSeconds = TotalTimeSeconds,
                 DistanceMeters = DistanceMeters,
                 MaximumSpeed = MaximumSpeed, Calories = Calories,
                 AverageHeartRateBpm = AverageHeartRateBpm,
                 MaximumHeartRateBpm = MaximumHeartRateBpm,
                 Intensity = Intensity, TriggerMethod = TriggerMethod,
                 ActivityId = ActivityId)

}
