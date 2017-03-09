#' Replace NA values in vector by closest element
#'
#' Replace NA with values from closest elemnt in vector. Stolen from
#' http://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value
#'
#' @param dat vector where NAs is to be replaced with value of closest element
#'
#' @return a vector of similar length as dat but without NAs. If dat contains no
#' or only NAs the input vector is returned unaltered
#' @export
#'
#' @examples
#'

replace_nas <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos

  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}
