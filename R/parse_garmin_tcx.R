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
#' @return  A list containg tibbles 'acti', meta' and 'data' in addition to the
#' file name of the actual input data and its md5 checksum
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
      acti <- parse_garmin_tcx_activity(sub_doc, ns)
      meta <- parse_garmin_tcx_metadata(sub_doc, ns)
      data <- parse_garmin_tcx_trackpoint(sub_doc, ns)
    } else {
      acti <- rbind(parse_garmin_tcx_activity(sub_doc, ns), acti)
      meta <- rbind(parse_garmin_tcx_metadata(sub_doc, ns), meta)
      data <- rbind(parse_garmin_tcx_trackpoint(sub_doc, ns), data)

    }
  }

  # postprocess
  data <- postprocess_track(meta = meta, data = data)

  # make checksum on tcx_file
  #md5sum <- digest::digest(tcx_file, algo = "md5", serialize = FALSE)
  md5sum <- tools::md5sum(tcx_file)

  # for debugging. To be removed
  print(paste("Stop parsing tcx at", Sys.time()))

  list(filename=basename(tcx_file), md5sum=md5sum, acti=acti, meta=meta,
      data=data)

}
