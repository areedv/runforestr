#' Provide geographical properties by spatial and temporal position
#'
#' @param lng Numeric decimal longitude
#' @param lat Numeric decimal latitude
#' @param datetime POSIX date and tome
#' @param property Character defining the property to return. One of 'name'
#' [default], 'temperature', 'precipitation', ...
#'
#' @return a list of properties
#' @export
#'
#' @examples
geo_property <- function(lng, lat, datetime = Sys.time(), property = "name") {

  # get config
  conf <- yaml::yaml.load_file(system.file("rfr.yml", package = "runforestr"))

  # build string
  apiUrl <- "http://api.geonames.org/"
  position <- paste0("lat=", lat, "&lng=", lng)
  if (property == "name") {
    service <- "findNearbyPlaceNameJSON?"
    username <- paste0("&username=", conf$api$geo$username)
  } else {
    stop("Currently only supports name lookup. Quitting...")
  }

  urls <- paste0(apiUrl, service, position, username)

  res <- jsonlite::fromJSON(urls)

  paste0(res$geonames$name, ", ", res$geonames$adminName1)

}
