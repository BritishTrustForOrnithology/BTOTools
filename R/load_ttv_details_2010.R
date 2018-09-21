#' Load visit information for Bird Atlas 2007-11 TTVs
#'
#' @description
#' Load the observer, date and duration information for all Timed Tetrad Visits
#' from Bird Atlas 2007-11.
#'
#' @param trim2010 Whether to remove data for local atlas TTVs completed outside the main Bird Atlas period (defaults to TRUE)
#'
#' @return A dataframe with the following columns:
#' \item{btoreg}{4-letter code for Atlas region}
#' \item{user_id}{Oracle observer username}
#' \item{tenkm}{10-km square}
#' \item{tetlet}{tetrad (A:Z minus O)}
#' \item{datestring}{visit date as a string, e.g. 19122007}
#' \item{source}{W=web submission; P=Paper submission}
#' \item{duration}{TTV duration: 1 or 2 hours}
#' \item{obsdt}{observation date as a Date object}
#'
#' @examples
#' df<-load_ttv_details_2010()
#'
#' @export
#'
load_ttv_details_2010<-function(trim2010=TRUE) {
  df<-read.fwf(paste0(.BTOarchive_path,'birdatlas2007-11/data/raw/tetrads_and_duration.lst')
                                ,header=F
                                ,widths=c(4,-1,32,-1,4,-3,1,-1,8,-1,1,-1,-2,1)
                                ,col.names=c('btoreg','user_id','tenkm','tetlet','datestring','source','duration')
                                ,colClasses=c(rep('character',times=6),'numeric'))
  #convert datestring to date value
  df$obsdt <- as.Date(df$datestring, "%d%m%Y")
  #optionally remove late data from local atlases
  if(trim2010==TRUE) df<-df[df$obsdt<as.Date("01/11/2011", format='%d/%m/%Y'),]
  return(df)
}
