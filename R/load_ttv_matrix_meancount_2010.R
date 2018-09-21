#' Load Bird Atlas 2007-11 species-by-visit matrix of mean TTV count across hours
#'
#' @description
#' A large matrix with TTV visits as rows, species as columns, giving the mean count across hours.
#' Species not detected on visits are populated with zero counts. Note that counts outside
#' the range have not been manipulated.
#'
#' @return
#' A dataframe with the following columns:
#' \item{user_id}{Oracle observer username}
#' \item{tenkm}{10-km square}
#' \item{tetlet}{tetrad letter (A:Z, minus O)}
#' \item{obsyear}{year of visit}
#' \item{obsmonth}{month of visit}
#' \item{obsday}{day of visit}
#' \item{season}{Breeding (B) or Winter (W) season}
#' \item{X_###}{336 columns, one per species, containing mean counts. Columns names are numeric species codes with prefix X_ (e.g. X_105)}
#'
#' @examples
#' df<-load_ttv_matrix_firsthour_2010()
#'
#' @export
#'

load_ttv_matrix_meancount_2010<-function() {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/ttv_mean_count_per_hour.csv'),
                 header=T,sep=",",
                 colClasses=c(rep('character',3),rep('numeric',3),'character',rep('numeric',347) )
  )
  names(df)[3]<-'tetlet'
  return(df)
}
