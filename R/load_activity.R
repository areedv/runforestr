#' Title
#'
#' @return
#' @export
#'
#' @examples

load_activity <- function() {

  filename <- paste0(conf$store$local$path, conf$store$filename)

  if (file.exists(filename)) {
    dat <- readRDS(filename)
    return(dat)
  } else {
    return(NULL)
  }
}
