#' Initialise the package including detect environment and load default paths
.onLoad<-function(libname=find.package('birdatlas'),pkgname='birdatlas') {
  cat("Birdatlas version 0.1.1 - access and manipulate bird atlas data")
  if(.Platform$OS.type =='windows') assign('.BTOarchive_path','//btodomain/FILES/UNIXArchive/',1)
  if(.Platform$OS.type=='unix') assign('.BTOarchive_path','/archive/',1)
}
