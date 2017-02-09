
#' Title
#'
#' @param tcx_file
#'
#' @return
#' @export
#'
#' @examples

parse_garmin_tcx <- function(tcx_file) {

  # these sould rather be part of some config file
  ActivityXPath <- "/TrainingCenterDatabase/Activities/Activity"

  doc <- xml2::read_xml(tcx_file)
  ns <- xml2::xml_ns(doc)

  # how many activities?
  n <- doc %>%
    xml2::xml_find_all(ActivityXPath, ns) %>%
    length()

  if (n < 0) {
    stop("There are no activities in the tcx-file")
  }

  for (i in 1:n) {
    xpath <- paste0(ActivityXPath, "[", i, "]/*")
    sub_doc <- doc %>%
      xml2::xml_find_all(xpath, ns)
    if (i == 1) {
      meta <- parse_garmin_tcx_metadata(sub_doc, ns)
      #data <- parse_garmin_tcx_trackpoint(sub_doc)
    } else {
      meta <- cbind(parse_garmin_tcx_metadata(sub_doc, ns))
      #data <- cbind(parse_garmin_tcx_trackpoint(sub_doc))

    }
  }

  #list(meta=meta, data=data)
  meta
}
