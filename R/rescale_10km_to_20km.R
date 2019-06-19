#' Convert a 10-km grid reference to a 20-km grid reference
#'
#' @description
#' Takes a dataframe of British National grid 10-km references and converts to 20-km references.
#' Works for Britain, Ireland and Channel Isles grids
#'
#' @param df name of input dataframe
#' @param invar name of the variable containing the input grid reference
#'
#' @return The same dataframe with additional segref column.
#'
#' @examples
#' temp1<-data.frame(cbc_code = c('R.','CH'), count = c(10,2), tenkm = c('TL13', 'SK87'), stringsAsFactors = FALSE)
#' rescale_10km_to_20km(temp1,'tenkm')
#'
#' @export
#'
rescale_10km_to_20km<-function(df,invar) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar)) stop('invar must be supplied as a character string')
  
  #check there isn't already a segref column
  if('segref' %in% names(df)) stop('df already contains a column called segref')
  
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
  
  #force name to be tenkm to make easier processing
  temp_df$let<-substr(temp_df$tenkm,1,2)
  temp_df$east<-as.numeric(substr(temp_df$tenkm,3,3))
  temp_df$north<-as.numeric(substr(temp_df$tenkm,4,4))
  temp_df$segref<-NA
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==0,'_A',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==1,'_A',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==0,'_A',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==1,'_A',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==0,'_F',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==1,'_F',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==0,'_F',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==1,'_F',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==0,'_K',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==1,'_K',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==0,'_K',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==1,'_K',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==0,'_Q',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==1,'_Q',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==0,'_Q',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==1,'_Q',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==0,'_V',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==1,'_V',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==0,'_V',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==1,'_V',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==2,'_B',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==3,'_B',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==2,'_B',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==3,'_B',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==2,'_G',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==3,'_G',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==2,'_G',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==3,'_G',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==2,'_L',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==3,'_L',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==2,'_L',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==3,'_L',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==2,'_R',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==3,'_R',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==2,'_R',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==3,'_R',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==2,'_W',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==3,'_W',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==2,'_W',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==3,'_W',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==4,'_C',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==5,'_C',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==4,'_C',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==5,'_C',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==4,'_H',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==5,'_H',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==4,'_H',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==5,'_H',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==4,'_M',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==5,'_M',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==4,'_M',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==5,'_M',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==4,'_S',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==5,'_S',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==4,'_S',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==5,'_S',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==4,'_X',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==5,'_X',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==4,'_X',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==5,'_X',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==6,'_D',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==7,'_D',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==6,'_D',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==7,'_D',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==6,'_I',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==7,'_I',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==6,'_I',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==7,'_I',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==6,'_N',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==7,'_N',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==6,'_N',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==7,'_N',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==6,'_T',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==7,'_T',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==6,'_T',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==7,'_T',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==6,'_Y',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==7,'_Y',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==6,'_Y',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==7,'_Y',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==8,'_E',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==0 & temp_df$north==9,'_E',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==8,'_E',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==1 & temp_df$north==9,'_E',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==8,'_J',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==2 & temp_df$north==9,'_J',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==8,'_J',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==3 & temp_df$north==9,'_J',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==8,'_P',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==4 & temp_df$north==9,'_P',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==8,'_P',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==5 & temp_df$north==9,'_P',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==8,'_U',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==6 & temp_df$north==9,'_U',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==8,'_U',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==7 & temp_df$north==9,'_U',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==8,'_Z',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==8 & temp_df$north==9,'_Z',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==8,'_Z',temp_df$segref)
  temp_df$segref<-ifelse(temp_df$east==9 & temp_df$north==9,'_Z',temp_df$segref)
  temp_df$segref<-paste0(temp_df$let,temp_df$segref)
  temp_df$let<-NULL
  temp_df$east<-NULL
  temp_df$north<-NULL
  temp_df$tenkm <- NULL
  #check df and temp_df still same length
  if(nrow(df) != nrow(temp_df)) stop('processed segref list not same length as original df')
  
  #add to original df and return
  df <- cbind(df, temp_df)
  return(df)
}
