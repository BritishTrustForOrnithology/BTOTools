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
  
  #convert using the vector method
  tetrad_id <- rescale_1km_to_2km_(df[,invar])
  
  #add to original df and return
  df <- cbind(df, tetrad_id)
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