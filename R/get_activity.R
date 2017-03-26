#' Title
#'
#' @param data
#' @param id
#'
#' @return
#' @export
#'
#' @examples
get_activity <- function(data, id) {

  d <- data

  d$acti <- dplyr::filter(d$acti, ActivityId %in% id)
  d$meta <- dplyr::filter(d$meta, ActivityId %in% id)
  d$data <- dplyr::filter(d$data, ActivityId %in% id)

  d

}
