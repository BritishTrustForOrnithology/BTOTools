#' Change path for archived data files
#'
#' @description
#' On load the package detects the operating environment and sets the path root
#' for the Unix archive space (for windows this is "//btodomain/FILES/UNIXArchive/").
#' Use this function to reset the path for instances where you are using data from
#' a different location. Note that you will still need data in the same folder
#' structure below this path level (e.g. ../birdatlas2007-11/data/processesd..).
#'
#' @param newpath
#'
#' @examples
#' change_archive_path('C:/local_data_copies/')
#' @export
#'
change_archive_path<-function(newpath) {
  assign('.BTOarchive_path',newpath,1)
}
