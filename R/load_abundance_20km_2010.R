#' Load 20-km relative abundance data for Bird Atlas 2007-11
#'
#' @description
#' Summarised relative abundance data from the Bird Atlas 2007-11 for all species.
#' Data are all at 20-km resolution and as such give confidential locations
#' for some rare breeding species. Note that breeding season abundance has been
#' removed for 10-km squares where there was no breeding evidence (e.g. for migrants).
#'
#' @param season Whether to return data for winter (W), breeding season (B) or both (BW)
#'
#' @return A dataframe with the following columns:
#' \item{segref}{20-km grid reference}
#' \item{season}{Breeding (B) or Winter (W)}
#' \item{speccode}{numeric species code}
#' \item{ntets}{number of tetrads surveyed in 20-km}
#' \item{freqindex}{proportion of tetrads in 20-km occupied}
#' \item{meancount}{mean of counts (averaged across hours, then across visits)}
#'
#' @examples
#' df<-load_abundance_20km_2010(season='B')
#'
#' @export
load_abundance_20km_2010<-function(season='BW') {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/results_abundbysp2010_20km.csv'),
                   header=T,sep=",",
                   colClasses=c('character','character','numeric','numeric','numeric','numeric' ))
  #reorder columns
  df<-df[,c(1,2,3,4,6,5)]
  #optionally filter on season
  if(season=='B') df<-df[df$season=='B',]
  if(season=='W') df<-df[df$season=='W',]
  return(df)
}
