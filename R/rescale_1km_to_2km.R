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
#' temp1a<-rescale_1km_to_2km(temp1,'onekm')
#' temp2<-data.frame(cbc_code='R.',count=10,onekm='TL123', stringsAsFactors = FALSE)
#' temp2a<-rescale_1km_to_2km(temp2,'onekm')
#' temp3<-data.frame(cbc_code='R.',count=10,onekm='TL12345', stringsAsFactors = FALSE)
#' temp3a<-rescale_1km_to_2km(temp3,'onekm')
#'
#' @export
#'
rescale_1km_to_2km<-function(df,invar) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar)) stop('invar must be supplied as a character string')
  
  #check there isn't already a tetrad_id column
  if('tetrad_id' %in% names(df)) stop('df already contains a column called tetrad_id')
  
  #which column to process
  invar_index<-which(names(df)== invar)

  #copy to new df for processing so as not to overwrite any columns in original df
  temp_df <- df[invar_index]
    
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
