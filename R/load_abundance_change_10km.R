#' Load 10-km relative abundance change data (1988-91 to 2008-11)
#'
#' @description
#' A measure of relative abundance change based on the difference between "frequency indices"
#' from the 1988-91 and 2008-11 atlas datasets. Note, although this dataset is calculated on
#' a 10-km grid, this means there are 10-km squares in Ireland that do not have data because
#' they were not surveyed in 2008-11. If the focus is on Britain and Ireland you may be best
#' using the 20-km eqivalent dataset. Alternatively, you can use this dataset safely for
#' Britain.
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{cbc_code}{2-letter species code}
#' \item{index90}{proportion of tetrads occupied in 1988-91}
#' \item{index10}{proportion of tetrads occupied in 2008-11}
#' \item{diff}{arithmetic difference in proportion of tetrads occupuied between atlases}
#' \item{easting}{British National grid easting}
#' \item{northing}{British National grid northing}
#'
#' @examples
#' df<-load_abundance_change_10km()
#'
#' @export
#'
load_abundance_change_10km<-function() {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/results_abchange_by_spp_10km.csv'),
                   header=T,sep=",",
                   colClasses=c('character','character',rep('numeric',5 )))
  return(df)
}
