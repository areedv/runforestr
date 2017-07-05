#' Title
#'
#' @return
#' @export
#'
#' @examples

load_activity <- function() {

  dbFilePath <- file.path(conf$store$dropbox$path, conf$store$filename)
  tempFilePath <- file.path(tempdir(), conf$store$filename)

  if (conf$store$dropbox$use) {
    rdrop2::drop_auth()
    if (rdrop2::drop_exists(path = dbFilePath)) {
      rdrop2::drop_get(path = dbFilePath, local_file = tempFilePath,
                       overwrite = TRUE)
    }
  }

  filename <- file.path(conf$store$local$path, conf$store$filename)

  if (file.exists(tempFilePath)) {
    dat <- readRDS(tempFilePath)
    return(dat)
  } else {
    return(NULL)
  }
}
