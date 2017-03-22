#' Parse xml file from Garmin (tcx)
#'
#' Provide track and corresponding meta data. Currently some post processing
#' is also performed
#'
#' For testing, use:
#' t <- parse_garmin_tcx(system.file("extdata/activity_1554734944.tcx", package = "runforestr"))
#'
#' @param tcx_file
#'
#' @return list containg tibbles 'meta' and 'data'
#' @export
#'
#' @examples

parse_garmin_tcx <- function(tcx_file) {

  # for debugging. To be removed
  print(paste("Start parsing tcx at", Sys.time()))

  # these should rather be part of some config file
  ActivityXPath <- "/d1:TrainingCenterDatabase/d1:Activities/d1:Activity"

  doc <- xml2::read_xml(tcx_file)
  ns <- xml2::xml_ns(doc)

  # how many activities?
  n <- doc %>%
    xml2::xml_find_all(ActivityXPath, ns) %>%
    length()

  if (n == 0) {
    stop(paste("There are no activities in", tcx_file))
  }

  for (i in 1:n) {
    xpath <- paste0(ActivityXPath, "[", i, "]/*")
    sub_doc <- doc %>%
      xml2::xml_find_all(xpath, ns)
    if (i == 1) {
      meta <- parse_garmin_tcx_metadata(sub_doc, ns)
      data <- parse_garmin_tcx_trackpoint(sub_doc, ns)
    } else {
      meta <- cbind(parse_garmin_tcx_metadata(sub_doc, ns), meta)
      data <- cbind(parse_garmin_tcx_trackpoint(sub_doc, ns), data)

    }
  }

  # postprocess
  data <- postprocess_track(meta = meta, data = data)

  # for debugging. To be removed
  print(paste("Stop parsing tcx at", Sys.time()))

  list(meta=meta, data=data)
}
