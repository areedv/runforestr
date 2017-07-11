#' Title
#'
#' @param data
#' @param range
#'
#' @return
#' @export
#'
#' @examples
get_timerange <- function(data, range) {

  d <- data

  print(range)

  d$acti <- dplyr::filter(d$acti, as.Date(DateTime) >= range[1] &
                          as.Date(DateTime) <= range[2])
  d$meta <- dplyr::filter(d$meta, ActivityId %in% d$acti$ActivityId)
  d$data <- dplyr::filter(d$data, ActivityId %in% d$acti$ActivityId)

  d
}
