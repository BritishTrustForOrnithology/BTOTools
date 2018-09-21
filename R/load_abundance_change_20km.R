#' Load 20-km relative abundance change data (1988-91 to 2008-11)
#'
#' @description
#' A measure of relative abundance change based on the difference between "frequency indices"
#' from the 1988-91 and 2008-11 atlas datasets. Calculated on a 20-km grid to allow the same
#' resolution of data presentation for Britain and Ireland (where there was a lower intensity
#' of sampling in 2008-11).
#'
#' @return A dataframe with the following columns:
#' \item{segref}{20-km grid reference}
#' \item{cbc_code}{2-letter species code}
#' \item{index90}{proportion of tetrads occupied in 1988-91}
#' \item{index10}{proportion of tetrads occupied in 2008-11}
#' \item{diff}{arithmetic difference in proportion of tetrads occupuied between atlases}
#' \item{easting}{British National grid easting}
#' \item{northing}{British National grid northing}
#'
#' @examples
#' df<-load_abundance_change_20km()
#'
#' @export
#'
load_abundance_change_20km<-function() {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/results_abchange_by_spp_20km.csv'),
                   header=T,sep=",",
                   colClasses=c('character','character',rep('numeric',6 )))
  df$cat<-NULL
  return(df)
}
