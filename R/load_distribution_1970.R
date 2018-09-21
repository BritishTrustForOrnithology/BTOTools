#' Load 10-km distribution data for the 1968-72 Breeding Atlas
#'
#' @description
#' Summarised distribution data from the 1968-72 breeding atlas for all species.
#' Data are all at 10-km resolution and as such give confidential locations
#' for some rare breeding species; for these species there may be a different square
#' in the "published" column. These data can be used to
#' replicate distribution maps or for analyses. Note some information is NA, e.g.  where we
#' records were apparently not published.
#'
#' @return A dataframe with the following columns:
#' \item{speccode}{numeric species code}
#' \item{cbc_code}{two-letter species code (for those species that have one, otherwise NA)}
#' \item{tenkm}{10-km grid reference of record}
#' \item{tenkm_published}{10-km grid reference as used in the book which may have been moved for rare breeding birds}
#' \item{cat}{breeding status (1 = possible breeding, 2 = probable breeding, 3 = confirmed breeding)}
#'
#' @examples
#' df<-load_distribution_1970()
#'
#' @export
load_distribution_1970<-function() {
  df<-read.fwf(paste0(.BTOarchive_path,'/birdatlas1968-72breed/sharrockbysquare.txt'),
                 header=F,
                 widths=c(5,-1,2,-1,4,-1,4,-1,1),
                 col.names = c('speccode','cbc_code','tenkm','tenkm_published','cat'),
                 colClasses=c('numeric',rep('character',3),'numeric')
  )
  #convert white space to NAs
  df$cbc_code<-ifelse(df$cbc_code=='  ',NA,df$cbc_code)
  df$tenkm_published<-ifelse(df$tenkm_published=='    ',NA,df$tenkm_published)
  return(df)
}

