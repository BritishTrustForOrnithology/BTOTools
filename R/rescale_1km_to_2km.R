#' Convert a 1-km grid reference to a 2-km (tetrad) grid reference
#'
#' @description
#' Takes a dataframe of British National grid 1-km references and converts to 2-km references
#' (i.e. tetrads). Works for Britain, Ireland and Channel Isles grids
#'
#' @param df name of input dataframe
#' @param invar name of the variable containing the input grid reference
#'
#' @return The same dataframe with additional tetrad_id column.
#'
#' @examples
#' temp1<-data.frame(cbc_code='R.',count=10,onekm='TL1234', stringsAsFactors = FALSE)
#' rescale_1km_to_2km(temp1,'onekm')
#'
#' @export
#'
rescale_1km_to_2km<-function(df,invar) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar)) stop('invar must be supplied as a character string')
  
  #check there isn't already a tetrad_id column
  if('tetrad_id' %in% names(df)) stop('df already contains a column called tetrad_id')
  
  #copy to new df for processing so as not to overwrite any columns in original df
  temp_df <- df[invar]
    
  #force name to be gridref to make easier processing
  names(temp_df)<-'gridref'

  #check if any values in the column are not 1-km refs
  nc <- nchar(temp_df$gridref)
  short <- min(nc, na.rm = TRUE)
  long <- max(nc, na.rm = TRUE)
  if(short < 6) stop('invar contains grid references that are too short')
  if(long > 6) stop('invar contains grid references that are too long')
  
  temp_df$let<-substr(temp_df$gridref,1,2)
  temp_df$e10<-substr(temp_df$gridref,3,3)
  temp_df$e1<-substr(temp_df$gridref,4,4)
  temp_df$n10<-substr(temp_df$gridref,5,5)
  temp_df$n1<-substr(temp_df$gridref,6,6)
  temp_df$tet<-'o'
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==0,'A',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==1,'A',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==0,'A',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==1,'A',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==0,'F',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==1,'F',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==0,'F',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==1,'F',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==0,'K',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==1,'K',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==0,'K',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==1,'K',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==0,'Q',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==1,'Q',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==0,'Q',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==1,'Q',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==0,'V',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==1,'V',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==0,'V',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==1,'V',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==2,'B',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==3,'B',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==2,'B',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==3,'B',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==2,'G',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==3,'G',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==2,'G',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==3,'G',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==2,'L',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==3,'L',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==2,'L',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==3,'L',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==2,'R',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==3,'R',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==2,'R',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==3,'R',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==2,'W',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==3,'W',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==2,'W',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==3,'W',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==4,'C',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==5,'C',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==4,'C',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==5,'C',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==4,'H',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==5,'H',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==4,'H',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==5,'H',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==4,'M',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==5,'M',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==4,'M',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==5,'M',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==4,'S',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==5,'S',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==4,'S',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==5,'S',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==4,'X',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==5,'X',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==4,'X',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==5,'X',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==6,'D',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==7,'D',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==6,'D',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==7,'D',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==6,'I',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==7,'I',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==6,'I',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==7,'I',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==6,'N',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==7,'N',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==6,'N',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==7,'N',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==6,'T',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==7,'T',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==6,'T',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==7,'T',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==6,'Y',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==7,'Y',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==6,'Y',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==7,'Y',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==8,'E',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==0 & temp_df$n1==9,'E',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==8,'E',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==1 & temp_df$n1==9,'E',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==8,'J',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==2 & temp_df$n1==9,'J',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==8,'J',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==3 & temp_df$n1==9,'J',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==8,'P',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==4 & temp_df$n1==9,'P',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==8,'P',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==5 & temp_df$n1==9,'P',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==8,'U',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==6 & temp_df$n1==9,'U',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==8,'U',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==7 & temp_df$n1==9,'U',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==8,'Z',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==8 & temp_df$n1==9,'Z',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==8,'Z',temp_df$tet)
  temp_df$tet<-ifelse(temp_df$e1==9 & temp_df$n1==9,'Z',temp_df$tet)
  temp_df$tetrad_id<-paste(temp_df$let,temp_df$e10,temp_df$n10,temp_df$tet,sep='')
  temp_df$let<-NULL
  temp_df$tet<-NULL
  temp_df$e10<-NULL
  temp_df$e1<-NULL
  temp_df$n10<-NULL
  temp_df$n1<-NULL
  temp_df$nc <- NULL
  temp_df$gridref <- NULL
  
  #check df and temp_df still same length
  if(nrow(df) != nrow(temp_df)) stop('processed tetrad list not same length as original df')
  
  #add to original df and return
  df <- cbind(df, temp_df)
  return(df)
}


#' Convert a 1-km grid reference to a 2-km (tetrad) grid reference
#'
#' @description
#' Takes a vector of British National grid 1-km references and converts to 2-km references
#' (i.e. tetrads). Works for Britain, Ireland and Channel Isles grids
#'
#' @param x string input variable
#' 
#' @return A vector with tetrad references.
#'
#' @examples
#' temp1<-data.frame(cbc_code='R.',count=10,onekm='TL1234', stringsAsFactors = FALSE)
#' rescale_1km_to_2km_(temp1$onekm)
#'
#' @export
#'
rescale_1km_to_2km_<-function(x) {
  #check input parameters
  if(!is.character(x)) stop('x must be supplied as a character string')
  
  #check if any values in the column are not 1-km refs
  nc <- nchar(x)
  short <- min(nc, na.rm = TRUE)
  long <- max(nc, na.rm = TRUE)
  if(short < 6) stop('x contains grid references that are too short')
  if(long > 6) stop('x contains grid references that are too long')
  
  let<-substr(x,1,2)
  e10<-substr(x,3,3)
  e1<-substr(x,4,4)
  n10<-substr(x,5,5)
  n1<-substr(x,6,6)
  tet<-'o'
  tet<-ifelse(e1==0 & n1==0,'A',tet)
  tet<-ifelse(e1==0 & n1==1,'A',tet)
  tet<-ifelse(e1==1 & n1==0,'A',tet)
  tet<-ifelse(e1==1 & n1==1,'A',tet)
  tet<-ifelse(e1==2 & n1==0,'F',tet)
  tet<-ifelse(e1==2 & n1==1,'F',tet)
  tet<-ifelse(e1==3 & n1==0,'F',tet)
  tet<-ifelse(e1==3 & n1==1,'F',tet)
  tet<-ifelse(e1==4 & n1==0,'K',tet)
  tet<-ifelse(e1==4 & n1==1,'K',tet)
  tet<-ifelse(e1==5 & n1==0,'K',tet)
  tet<-ifelse(e1==5 & n1==1,'K',tet)
  tet<-ifelse(e1==6 & n1==0,'Q',tet)
  tet<-ifelse(e1==6 & n1==1,'Q',tet)
  tet<-ifelse(e1==7 & n1==0,'Q',tet)
  tet<-ifelse(e1==7 & n1==1,'Q',tet)
  tet<-ifelse(e1==8 & n1==0,'V',tet)
  tet<-ifelse(e1==8 & n1==1,'V',tet)
  tet<-ifelse(e1==9 & n1==0,'V',tet)
  tet<-ifelse(e1==9 & n1==1,'V',tet)
  tet<-ifelse(e1==0 & n1==2,'B',tet)
  tet<-ifelse(e1==0 & n1==3,'B',tet)
  tet<-ifelse(e1==1 & n1==2,'B',tet)
  tet<-ifelse(e1==1 & n1==3,'B',tet)
  tet<-ifelse(e1==2 & n1==2,'G',tet)
  tet<-ifelse(e1==2 & n1==3,'G',tet)
  tet<-ifelse(e1==3 & n1==2,'G',tet)
  tet<-ifelse(e1==3 & n1==3,'G',tet)
  tet<-ifelse(e1==4 & n1==2,'L',tet)
  tet<-ifelse(e1==4 & n1==3,'L',tet)
  tet<-ifelse(e1==5 & n1==2,'L',tet)
  tet<-ifelse(e1==5 & n1==3,'L',tet)
  tet<-ifelse(e1==6 & n1==2,'R',tet)
  tet<-ifelse(e1==6 & n1==3,'R',tet)
  tet<-ifelse(e1==7 & n1==2,'R',tet)
  tet<-ifelse(e1==7 & n1==3,'R',tet)
  tet<-ifelse(e1==8 & n1==2,'W',tet)
  tet<-ifelse(e1==8 & n1==3,'W',tet)
  tet<-ifelse(e1==9 & n1==2,'W',tet)
  tet<-ifelse(e1==9 & n1==3,'W',tet)
  tet<-ifelse(e1==0 & n1==4,'C',tet)
  tet<-ifelse(e1==0 & n1==5,'C',tet)
  tet<-ifelse(e1==1 & n1==4,'C',tet)
  tet<-ifelse(e1==1 & n1==5,'C',tet)
  tet<-ifelse(e1==2 & n1==4,'H',tet)
  tet<-ifelse(e1==2 & n1==5,'H',tet)
  tet<-ifelse(e1==3 & n1==4,'H',tet)
  tet<-ifelse(e1==3 & n1==5,'H',tet)
  tet<-ifelse(e1==4 & n1==4,'M',tet)
  tet<-ifelse(e1==4 & n1==5,'M',tet)
  tet<-ifelse(e1==5 & n1==4,'M',tet)
  tet<-ifelse(e1==5 & n1==5,'M',tet)
  tet<-ifelse(e1==6 & n1==4,'S',tet)
  tet<-ifelse(e1==6 & n1==5,'S',tet)
  tet<-ifelse(e1==7 & n1==4,'S',tet)
  tet<-ifelse(e1==7 & n1==5,'S',tet)
  tet<-ifelse(e1==8 & n1==4,'X',tet)
  tet<-ifelse(e1==8 & n1==5,'X',tet)
  tet<-ifelse(e1==9 & n1==4,'X',tet)
  tet<-ifelse(e1==9 & n1==5,'X',tet)
  tet<-ifelse(e1==0 & n1==6,'D',tet)
  tet<-ifelse(e1==0 & n1==7,'D',tet)
  tet<-ifelse(e1==1 & n1==6,'D',tet)
  tet<-ifelse(e1==1 & n1==7,'D',tet)
  tet<-ifelse(e1==2 & n1==6,'I',tet)
  tet<-ifelse(e1==2 & n1==7,'I',tet)
  tet<-ifelse(e1==3 & n1==6,'I',tet)
  tet<-ifelse(e1==3 & n1==7,'I',tet)
  tet<-ifelse(e1==4 & n1==6,'N',tet)
  tet<-ifelse(e1==4 & n1==7,'N',tet)
  tet<-ifelse(e1==5 & n1==6,'N',tet)
  tet<-ifelse(e1==5 & n1==7,'N',tet)
  tet<-ifelse(e1==6 & n1==6,'T',tet)
  tet<-ifelse(e1==6 & n1==7,'T',tet)
  tet<-ifelse(e1==7 & n1==6,'T',tet)
  tet<-ifelse(e1==7 & n1==7,'T',tet)
  tet<-ifelse(e1==8 & n1==6,'Y',tet)
  tet<-ifelse(e1==8 & n1==7,'Y',tet)
  tet<-ifelse(e1==9 & n1==6,'Y',tet)
  tet<-ifelse(e1==9 & n1==7,'Y',tet)
  tet<-ifelse(e1==0 & n1==8,'E',tet)
  tet<-ifelse(e1==0 & n1==9,'E',tet)
  tet<-ifelse(e1==1 & n1==8,'E',tet)
  tet<-ifelse(e1==1 & n1==9,'E',tet)
  tet<-ifelse(e1==2 & n1==8,'J',tet)
  tet<-ifelse(e1==2 & n1==9,'J',tet)
  tet<-ifelse(e1==3 & n1==8,'J',tet)
  tet<-ifelse(e1==3 & n1==9,'J',tet)
  tet<-ifelse(e1==4 & n1==8,'P',tet)
  tet<-ifelse(e1==4 & n1==9,'P',tet)
  tet<-ifelse(e1==5 & n1==8,'P',tet)
  tet<-ifelse(e1==5 & n1==9,'P',tet)
  tet<-ifelse(e1==6 & n1==8,'U',tet)
  tet<-ifelse(e1==6 & n1==9,'U',tet)
  tet<-ifelse(e1==7 & n1==8,'U',tet)
  tet<-ifelse(e1==7 & n1==9,'U',tet)
  tet<-ifelse(e1==8 & n1==8,'Z',tet)
  tet<-ifelse(e1==8 & n1==9,'Z',tet)
  tet<-ifelse(e1==9 & n1==8,'Z',tet)
  tet<-ifelse(e1==9 & n1==9,'Z',tet)
  tetrad_id<-paste(let,e10,n10,tet,sep='')
  let<-NULL
  tet<-NULL
  e10<-NULL
  e1<-NULL
  n10<-NULL
  n1<-NULL
  nc <- NULL
  
  #check result and input are the same length
  if(length(tetrad_id) != length(x)) stop('processed tetrad list not same length as original df')
  
  return(tetrad_id)
}