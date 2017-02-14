#' Title
#'
#' @param data
#' @param t0
#' @param t1
#'
#' @return
#' @export
#'
#' @examples

r_map <- function(data, t0, t1) {

  if(missing(t0)) {
    t0 <- min(data$Time)
  }

  if (missing(t1)) {
    t1 <- max(data$Time)
  }

  # process data
  data <- dplyr::filter(.data = data, Time >= t0 & Time <= t1)

  list(data=data)
}
