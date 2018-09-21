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
#' temp<-data.frame(cbc_code='R.',count=10,tenkm='TL13')
#' temp2<-rescale_10km_to_20km(temp,'tenkm')
#' print(temp2)
#' cbc_code count  tenkm segref
#'       R.    10   TL13   TL_B
#'
#' @export
#'
rescale_10km_to_20km<-function(df,invar) {
  #which column to process
  invar.index<-which(names(df)== invar)
  #force name to be tenkm to make easier processing
  names(df)[invar.index]<-'tenkm'
  df$let<-substr(df$tenkm,1,2)
  df$east<-as.numeric(substr(df$tenkm,3,3))
  df$north<-as.numeric(substr(df$tenkm,4,4))
  df$segref<-NA
  df$segref<-ifelse(df$east==0 & df$north==0,'_A',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==1,'_A',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==0,'_A',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==1,'_A',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==0,'_F',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==1,'_F',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==0,'_F',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==1,'_F',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==0,'_K',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==1,'_K',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==0,'_K',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==1,'_K',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==0,'_Q',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==1,'_Q',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==0,'_Q',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==1,'_Q',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==0,'_V',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==1,'_V',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==0,'_V',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==1,'_V',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==2,'_B',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==3,'_B',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==2,'_B',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==3,'_B',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==2,'_G',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==3,'_G',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==2,'_G',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==3,'_G',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==2,'_L',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==3,'_L',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==2,'_L',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==3,'_L',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==2,'_R',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==3,'_R',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==2,'_R',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==3,'_R',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==2,'_W',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==3,'_W',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==2,'_W',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==3,'_W',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==4,'_C',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==5,'_C',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==4,'_C',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==5,'_C',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==4,'_H',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==5,'_H',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==4,'_H',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==5,'_H',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==4,'_M',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==5,'_M',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==4,'_M',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==5,'_M',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==4,'_S',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==5,'_S',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==4,'_S',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==5,'_S',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==4,'_X',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==5,'_X',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==4,'_X',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==5,'_X',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==6,'_D',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==7,'_D',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==6,'_D',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==7,'_D',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==6,'_I',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==7,'_I',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==6,'_I',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==7,'_I',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==6,'_N',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==7,'_N',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==6,'_N',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==7,'_N',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==6,'_T',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==7,'_T',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==6,'_T',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==7,'_T',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==6,'_Y',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==7,'_Y',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==6,'_Y',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==7,'_Y',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==8,'_E',df$segref)
  df$segref<-ifelse(df$east==0 & df$north==9,'_E',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==8,'_E',df$segref)
  df$segref<-ifelse(df$east==1 & df$north==9,'_E',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==8,'_J',df$segref)
  df$segref<-ifelse(df$east==2 & df$north==9,'_J',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==8,'_J',df$segref)
  df$segref<-ifelse(df$east==3 & df$north==9,'_J',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==8,'_P',df$segref)
  df$segref<-ifelse(df$east==4 & df$north==9,'_P',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==8,'_P',df$segref)
  df$segref<-ifelse(df$east==5 & df$north==9,'_P',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==8,'_U',df$segref)
  df$segref<-ifelse(df$east==6 & df$north==9,'_U',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==8,'_U',df$segref)
  df$segref<-ifelse(df$east==7 & df$north==9,'_U',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==8,'_Z',df$segref)
  df$segref<-ifelse(df$east==8 & df$north==9,'_Z',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==8,'_Z',df$segref)
  df$segref<-ifelse(df$east==9 & df$north==9,'_Z',df$segref)
  df$segref<-paste0(df$let,df$segref)
  df$let<-NULL
  df$east<-NULL
  df$north<-NULL
  names(df)[invar.index]<-invar
  return(df)
}
