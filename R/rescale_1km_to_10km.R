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
#' temp<-data.frame(cbc_code='R.',count=10,onekm='TL1234')
#' temp2<-rescale_1km_to_10km(temp,'onekm')
#' print(temp2)
#' cbc_code count  onekm tenkm
#'       R.    10 TL1234  TL13
#'
#' @export
#'
rescale_1km_to_10km<-function(df,invar) {
  invar.index<-which(names(df)== invar)
  df$tenkm<-paste(substr(df[,invar.index],1,3),substr(df[,invar.index],5,5),sep=''  )
  return(df)
}
