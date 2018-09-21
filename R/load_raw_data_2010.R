#' Load Bird Atlas 2007-11 raw data in unsummarised format
#'
#' @description
#' Loads more-or-less raw data for all data types for the main 2007-11 fieldwork period
#' (i.e excludes any local atlas TTVs done in 2012-). The data are essentially as
#' submitted except for e.g. some cleaning of subspecies, reallocation of date for Used Nests
#' in winter to breeding season.
#'
#' @return A dataframe with the following columns:
#' \item{user_id}{Oracle username of observer}
#' \item{protocol_id}{data type (AROV = Atlas Roving Record, ATTV1 = 1st hour count; ATTV2 = 2nd hour count; ATTVC = colony count; ATTVE = casual records after timed bit of TTV; BTL = BirdTrack list; BTC = BirdTrack casual record)}
#' \item{tetlet}{tetrad letter}
#' \item{speccode}{numeric species code}
#' \item{how_many}{number of individuals encountered in hour}
#' \item{breedcode}{breeding evidence code (M, U, H, S, NE, FF etc)}
#' \item{app_occ_nests}{number of apparently occupied nests for colonial species (ATTVC protocol_id rows only)}
#' \item{obsday}{observation day}
#' \item{obsmonth}{observation month}
#' \item{obsyear}{observation year}
#' \item{season}{Breeding (B) or Winter (W) season}
#' \item{tenkm}{10-km square of tetrad}
#' \item{confidential}{is the record from a confidential source (1=yes, 0=no)}
#' \item{cat}{numeric code indicating highest recorded status from this hour/visit. For Breeding season, 0 = present, 0.01 = Flying, 0.1 = Migrant, 0.2 = Summering, 1 = Possible breeding, 2 = Probable breeding, 3 = Confirmed breeding. For winter, 0.01 = Flying, 3 = Present}
#'
#' @examples
#' df<-load_raw_data_2010()
#'
#' @export

load_raw_data_2010<-function() {
  cat('WARNING: This is a big file and will be slow to load!\n')
  df<-read.table(paste0(.BTOarchive_path,'/birdatlas2007-11/data/processed/extract.csv'),
                         header=T,
                         sep=',',
                         colClasses=c(rep('character',3),rep('numeric',2),'character',rep('numeric',4),rep('character',3),'numeric')
  )
  names(df)[3]<-'tetlet'
  return(df)
}
