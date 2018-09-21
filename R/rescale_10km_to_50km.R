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
#' temp<-data.frame(cbc_code='R.',count=10,tenkm='TL13')
#' temp2<-rescale_10km_to_50km(temp,'tenkm')
#' print(temp2)
#' cbc_code count  tenkm quadref
#'       R.    10   TL13    TLSW
#'
#' @export
#'
rescale_10km_to_50km<-function(df,invar) {
  #which column to process
  invar.index<-which(names(df)== invar)
  #force name to be tenkm to make easier processing
  names(df)[invar.index]<-'tenkm'
  let<-substr(df$tenkm,1,2)
  east<-as.numeric(substr(df$tenkm,3,3))
  north<-as.numeric(substr(df$tenkm,4,4))
  df$quadref<-NA
  df$quadref<-ifelse(east<=4 & north<=4, paste0(let,'SW'),df$quadref)
  df$quadref<-ifelse(east<=4 & north>4, paste0(let,'NW'),df$quadref)
  df$quadref<-ifelse(east>4 & north>4, paste0(let,'NE'),df$quadref)
  df$quadref<-ifelse(east>4 & north<=4, paste0(let,'SE'),df$quadref)
  df$let<-NULL
  df$east<-NULL
  df$north<-NULL
  names(df)[invar.index]<-invar
  return(df)
}
