#' Load 10-km distribution change data
#'
#' @description
#' The changes in occupancy of individual 10-km squares for different pairs of atlases (breeding to
#' breeding or winter to winter). Also a long-term comparison of change in occupancy across all three
#' breeding atlases. These data can be used to recreate the distributin change maps presented in the
#' book. Note that as these are the real data they can indicate locations of sensitive species. Hence
#' these data should not be distributed or mapped publicaly for such species. If in doubt, check how the
#' species was mapped in the book.
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{speccode}{numeric species code}
#' \item{cbc_code}{2-letter species code, for species that have one}
#' \item{season}{Breeding (B) or Winter (W)}
#' \item{interval}{the interval between atlases over which calculated: 1970-1990, 1970-2010,
#'                 1990-2010, 1980-2010 (i.e. winter) and 1970-1990-2010}
#' \item{change}{class of change: STA=occupied (i.e. stable) in both atlases, INC=occupancy
#'               gain, DEC=occupancy loss. For changes calculated across all three breeding
#'               atlases: MIDDEC=loss in 88-91, OLDDEC=loss in 1968-72, NEWDEC=loss in
#'               2008-11, MIDINC=gain in 88-91, OLDINC=gain in 1968-72, NEWINC=gain in 2008-11,
#'               STABLE=occupied in all three atlases.}
#'
#' @examples
#' df<-load_abundance_change_10km()
#'
#' @export
#'
load_distribution_change_10km<-function() {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/distribution_changes10km.csv'),
                   header=T,sep=",",
                   colClasses=c('character','numeric','character','character','character','character' ))
  names(df)[6]<-'cbc_code'
  df$cbc_code<-ifelse(df$cbc_code=='',NA,df$cbc_code)
  df<-df[,c(1,2,6,5,4,3)]
  return(df)
}
