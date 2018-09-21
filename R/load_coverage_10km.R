#' Load species codes and names as used in Bird Atlas 2007-11
#'
#' @description
#' A lookup file indicating whether individual 10-km squares were surveyed in pairs of atlas. This is
#' needed when calculating range changes as not all 10-km squares were surveyed in all atlases. As far
#' as possible this takes account of 10-km squares that were surveyed in an atlas but for which no species
#' were detected (such squares are therefore "missing" from the distributin datasets). Note also that some
#' 10-km squares are combined because for earlier atlases small islands (e.g. Fair Isle, individual
#' Channel Isles) were assigned to a single 10-km square even if they actually spanned more than one.
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km}
#' \item{breed1970to1990}{10-km covered in both atlases}
#' \item{breed1970to2010}{10-km covered in both atlases}
#' \item{breed1990to2010}{10-km covered in both atlases}
#' \item{breed197019902010}{10-km covered in all three atlases}
#' \item{winter1980to2010}{10-km covered in both atlases}
#'
#' @examples
#' df<-load_coverage_10km()
#'
#' @export
load_coverage_10km<-function() {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/coverage4change.csv'),
                   sep=',',
                   header=T,
                   colClasses=c('character',rep('numeric',5))
  )
  return(df)
}
