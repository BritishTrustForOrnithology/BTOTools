#' Load 10-km distribution data for the 1988-91 Breeding Atlas
#'
#' @description
#' Summarised distribution data from the 1988-91 breeding atlas for all species.
#' Data are all at 10-km resolution and as such give confidential locations
#' for some rare breeding species; for these species there may be a different square or
#' breeding evidence code in the "published" columns. These data can be used to
#' replicate distribution maps or for analyses. Note some information is NA, e.g.  where we
#' could not deduce true locations of moved records.
#'
#' @return A dataframe with the following columns:
#' \item{speccode}{numeric species code}
#' \item{cbc_code}{two-letter species code (for those species that have one, otherwise NA)}
#' \item{tenkm}{10-km grid reference of record}
#' \item{tenkm_published}{10-km grid reference as used in the book which may have been moved for rare breeding birds}
#' \item{cat}{breeding status (S = seen, B = Breeding)}
#' \item{cat_published}{breeding status as used in the book, which may have been downgraded or removed}
#'
#' @examples
#' df<-load_distribution_1990()
#'
#' @export
load_distribution_1990<-function() {
  df<-read.fwf(paste0(.BTOarchive_path,'birdatlas1988-91breed/data10km/gibbonsbysquare.txt'),
                 header=F,
                 widths=c(5,-1,2,-1,4,-1,4,-1,1,-1,1),
                 col.names = c('speccode','cbc_code','tenkm','tenkm_published','cat','cat_published'),
                 colClasses=c('numeric',rep('character',5))
  )
  #convert white space to NAs
  df$cbc_code<-ifelse(df$cbc_code=='  ',NA,df$cbc_code)
  df$tenkm<-ifelse(df$tenkm=='    ',NA,df$tenkm)
  df$tenkm_published<-ifelse(df$tenkm_published=='    ',NA,df$tenkm_published)
  df$cat<-ifelse(df$cat==' ',NA,df$cat)
  df$cat_published<-ifelse(df$cat_published==' ',NA,df$cat_published)
  return(df)
}



