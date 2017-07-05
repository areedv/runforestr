#' Title
#'
#' @param data a list containing filename, file md5 checksum, lists of
#' acti(vity data), meta(data) and data
#'
#' @return nothing, but stores a list of value-pairs and lists
#' @export
#'
#' @examples

save_activity <- function(data) {

  outfilename <- paste0(conf$store$local$path, conf$store$filename)
  dat <- load_activity()
  is_duplicate = FALSE

  if (!is.null(dat)) {
    if (data$md5sum %in% dat$md5sum) {
      is_duplicate = TRUE
      print("Data already exist. Doing nothing")
    } else {
      print("Appending to existing store")
      data$filename <- c(dat$filename, data$filename)
      data$md5sum <- c(dat$md5sum, data$md5sum)
      data$acti <- rbind(dat$acti, data$acti)
      data$meta <- rbind(dat$meta, data$meta)
      data$data <- rbind(dat$data, data$data)
    }
  }

  if (!is_duplicate){
    tempFilePath <- file.path(tempdir(), conf$store$filename)
    saveRDS(data, tempFilePath)
    if (conf$store$dropbox$use) {
      # need to check that directory exists, create if not
      dbDirs <- rdrop2::drop_dir()
      if (!conf$store$dropbox$path %in% basename(dbDirs$path)) {
        print("Creating new directory in Dropbox\n")
        rdrop2::drop_create(path = conf$store$dropbox$path)
      }
      rdrop2::drop_upload(file = tempFilePath, dest = conf$store$dropbox$path)
    }
    if (conf$store$local$use) {
      saveRDS(data, outfilename)
    }
  }
}
