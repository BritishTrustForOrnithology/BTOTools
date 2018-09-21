#' Load 10-km backwards-compatible distribution data for Bird Atlas 2010
#'
#' @description
#' Summarised distribution data from Bird Atlas 2007-11 for use in distribution change
#' calculations. Accordingly, maximum breeding evidence was calculated after removing any
#' out of season breeding evidence (which was not permitted in previous atlases). Also for
#' winter the data were limited to the same recording window (mid Nov onwards) as in the
#' 1980s Winter Atlas. Also some 10-km squares on small islands are combined,
#' e.g. HZ16,26,17,27 (Fair Isle) to make comparable with how data were collected in
#' previous atlases. It is still necessary to use coverage information to identify which
#' 10-km squares were surveyed in different atlases in order to generate valid changes.
#' Data are all at 10-km resolution and as such give confidential locations
#' for some rare breeding species.
#'
#' @param season Whether to return data for winter (W), breeding season (B) or both (BW)
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{season}{B or W for Breeding or Winter season}
#' \item{speccode}{numeric species code}
#' \item{cat}{numeric code indicating highest recorded status in 10-km square. For Breeding season, 1 = Possible breeding, 2 = Probable breeding, 3 = Confirmed breeding. For winter, 3 = Present}
#'
#' @examples
#' df<-load_distribution_2010_for_change(season='B')
#'
#' @export
#'
load_distribution_2010_for_change<-function(season='BW') {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/maxcat4change.csv'),
                   header=T,sep=",",
                   colClasses=c('character','character','numeric','numeric'))
  if(season=='B') df<-df[df$season=='B',]
  if(season=='W') df<-df[df$season=='W',]
  return(df)
}
