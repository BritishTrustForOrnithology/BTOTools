#' Load 10-km distribution data for Bird Atlas 2010
#'
#' @description
#' Summarised distribution data from Bird Atlas 2007-11 for all species, for winter and/or
#' the breeding season. Data are all at 10-km resolution and as such give confidential locations
#' for some rare breeding species. These data can be used to replicate distribution maps or for
#' analyses.
#'
#' @param season Whether to return data for winter (W), breeding season (B) or both (BW)
#' @param exclude_non_breeding Whether to exclude or include non-breeding records (Flying, Migrant, Summering) from the breeding season data
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{season}{B or W for Breeding or Winter season}
#' \item{speccode}{numeric species code}
#' \item{cat}{numeric code indicating highest recorded status in 10-km square. For Breeding season, 0 = present, 0.01 = Flying, 0.1 = Migrant, 0.2 = Summering, 1 = Possible breeding, 2 = Probable breeding, 3 = Confirmed breeding. For winter, 0.01 = Flying, 3 = Present}
#'
#' @examples
#' df<-load_distribution_2010(season='B', exclude_non_breeding=TRUE)
#'
#' @export
#'
load_distribution_2010<-function(season='BW',exclude_non_breeding=FALSE) {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/maxcat.csv'),
                   header=T,sep=",",
                   colClasses=c('character','character','numeric','numeric'))
  if(season=='B') df<-df[df$season=='B',]
  if(season=='W') df<-df[df$season=='W',]
  if(exclude_non_breeding==TRUE) df<-df[df$cat>=1,]
  return(df)
}
