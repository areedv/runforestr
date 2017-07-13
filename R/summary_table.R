#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

summary_table <- function(dat) {

  s <- dat$data %>%
    dplyr::group_by(ActivityId) %>%
    summarise(km = max(DistanceMeters/1000),
              dur = as.numeric(difftime(max(Time), min(Time),
                                        units = "mins")),
              maxHR = max(HeartRateBpm, na.rm=TRUE),
              mspeed = mean(Speed, na.rm=TRUE),
              mpace = mean(Pace, na.rm=TRUE))

  dplyr::inner_join(dat$acti, s, by="ActivityId")
}
