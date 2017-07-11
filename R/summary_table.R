#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

summary_table <- function(data) {
  s1 <- d$data %>%
    dplyr::group_by(ActivityId) %>%
    summarise(km=max(DistanceMeters/1000), maxHR=max(HeartRateBpm, na.rm=TRUE),
              mspeed=mean(Speed, na.rm=TRUE), mpace=mean(Pace, na.rm=TRUE))
  s <- dplyr::inner_join(d$acti, s1, by="ActivityId")

  tibble(Type=s$Sport, Time=strftime(s$DateTime, format="%F %R"), km=s$km,
         maxHR=s$maxHR, mSpeed=s$mspeed, mPace=s$mpace)
}
