#' Load species codes and names as used in Bird Atlas 2007-11
#'
#' @description
#' A lookup file of species codes and English and scientific names as used in Bird Atlas 2007-11. Note that
#' scientific names and taxonomic order has changed since.
#'
#' @return A dataframe with the following columns:
#' \item{speccode}{numeric species code}
#' \item{cbc_code}{two-letter species code (for those species that have one, otherwise NA)}
#' \item{engname}{species common name}
#' \item{sciname}{species scientific name}
#' \item{bou_order}{species order at time of Atlas}
#'
#' @examples
#' df<-load_specnames()
#'
#' @export
load_specnames<-function() {
  df<-read.table(paste0(.BTOarchive_path,'/birdatlas2007-11/data/lookups/specnames.csv'),
                   sep=',',
                   quote = "",
                   header=T,
                   colClasses=c('numeric','character','character','character','numeric')
  )
  df$cbc_code<-ifelse(df$cbc_code=='',NA,df$cbc_code)
  return(df)
}
