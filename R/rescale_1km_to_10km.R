#' Convert a 1-km grid reference to a 10-km grid reference
#'
#' @description
#' Takes a dataframe of British National grid 1-km references and converts to 10-km references.
#' Works for Britain, Ireland and Channel Isles grids
#'
#' @param df name of input dataframe
#' @param invar name of the variable containing the input grid reference
#'
#' @return The same dataframe with additional tenkm column
#'
#' @examples
#' temp1<-data.frame(cbc_code='R.',count=10,onekm='TL1234', stringsAsFactors = FALSE)
#' rescale_1km_to_10km(temp1,'onekm')
#'
#' @export
#'
rescale_1km_to_10km<-function(df,invar) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar)) stop('invar must be supplied as a character string')

  #check there isn't already a tenkm column
  if('tenkm' %in% names(df)) stop('df already contains a column called tenkm')
    
  invar.index<-which(names(df)== invar)
  
  #check if any values in the column are not 1-km refs
  nc <- nchar(df[,invar.index])
  short <- min(nc, na.rm = TRUE)
  long <- max(nc, na.rm = TRUE)
  if(short < 6) stop('invar contains grid references that are too short')
  if(long > 6) stop('invar contains grid references that are too long')
  
  df$tenkm<-paste(substr(df[,invar.index],1,3),substr(df[,invar.index],5,5),sep=''  )
  return(df)
}
