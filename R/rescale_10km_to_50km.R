#' Convert a 10-km grid reference to a 50-km grid reference
#'
#' @description
#' Takes a dataframe of British National grid 10-km references and converts to 50-km references.
#' Works for Britain, Ireland and Channel Isles grids
#'
#' @param df name of input dataframe
#' @param invar name of the variable containing the input grid reference
#'
#' @return The same dataframe with additional quadref column.
#'
#' @examples
#' temp1<-data.frame(cbc_code = 'R.', count = 10, tenkm = 'TL13', stringsAsFactors = FALSE)
#' rescale_10km_to_50km(temp1,'tenkm')
#'
#' @export
#'
rescale_10km_to_50km<-function(df,invar) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar)) stop('invar must be supplied as a character string')
  
  #check there isn't already a quadref column
  if('quadref' %in% names(df)) stop('df already contains a column called quadref')
  
  #copy to new df for processing so as not to overwrite any columns in original df
  temp_df <- df[invar]
  
  #force name to be tenkm to make easier processing
  names(temp_df)<-'tenkm'
  
  #check if any values in the column are not 1-km refs
  nc <- nchar(temp_df$tenkm)
  short <- min(nc, na.rm = TRUE)
  long <- max(nc, na.rm = TRUE)
  if(short < 4) stop('invar contains grid references that are too short')
  if(long > 4) stop('invar contains grid references that are too long')

  let<-substr(temp_df$tenkm,1,2)
  east<-as.numeric(substr(temp_df$tenkm,3,3))
  north<-as.numeric(substr(temp_df$tenkm,4,4))
  temp_df$quadref<-NA
  temp_df$quadref<-ifelse(east<=4 & north<=4, paste0(let,'SW'),temp_df$quadref)
  temp_df$quadref<-ifelse(east<=4 & north>4, paste0(let,'NW'),temp_df$quadref)
  temp_df$quadref<-ifelse(east>4 & north>4, paste0(let,'NE'),temp_df$quadref)
  temp_df$quadref<-ifelse(east>4 & north<=4, paste0(let,'SE'),temp_df$quadref)
  temp_df$let<-NULL
  temp_df$east<-NULL
  temp_df$north<-NULL
  temp_df$tenkm <- NULL
  
  #check df and temp_df still same length
  if(nrow(df) != nrow(temp_df)) stop('processed segref list not same length as original df')
  
  #add to original df and return
  df <- cbind(df, temp_df, stringsAsFactors = FALSE)
  return(df)
}
