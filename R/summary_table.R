#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

summary_table <- function(data) {

  # convert pace to mins:secs
  f <- function(decimal_mins) {
    as.character(decimal_mins * 60) %>%
      strptime(format = "%s") %>%
      format("%M:%S")
  }

  s1 <- data$data %>%
    dplyr::group_by(ActivityId) %>%
    summarise(km=max(DistanceMeters/1000), maxHR=max(HeartRateBpm, na.rm=TRUE),
              mspeed=mean(Speed, na.rm=TRUE), mpace=mean(Pace, na.rm=TRUE))
  s <- dplyr::inner_join(data$acti, s1, by="ActivityId")

  dplyr::tibble(Type=s$Sport, Time=strftime(s$DateTime, format="%F %R"),
                km=round(s$km, digits = 1), maxHR=s$maxHR,
                mSpeed=round(s$mspeed, digits = 1),
                mPace=f(s$mpace)) %>% arrange(desc(Time))
}
