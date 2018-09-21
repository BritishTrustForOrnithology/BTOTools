#' Initialise the package including detect environment and load default paths
.onLoad<-function(libname=find.package('BTOTools'),pkgname='BTOTools') {
  cat("BTOTools version 0.1.0 - access and manipulate BTO data")
  if(.Platform$OS.type =='windows') assign('.BTOarchive_path','//btodomain/FILES/UNIXArchive/',1)
  if(.Platform$OS.type=='unix') assign('.BTOarchive_path','/archive/',1)
}
