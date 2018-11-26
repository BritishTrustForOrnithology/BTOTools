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
#' temp<-data.frame(cbc_code='R.',count=10,onekm='TL1234')
#' temp2<-rescale_1km_to_2km(temp,'onekm')
#' print(temp2)
#' cbc_code count  onekm tetrad_id
#'       R.    10 TL1234    TL13H
#'
#' @export
#'
rescale_1km_to_2km<-function(df,invar) {
  #check input parameters
  if(!is.character(df)) stop('df must be supplied as a character string')
  if(!is.character(invar)) stop('invar must be supplied as a character string')
  
  #which column to process
  invar.index<-which(names(df)== invar)
  #force name to be gridref to make easier processing
  names(df)[invar.index]<-'gridref'
  df$let<-substr(df$gridref,1,2)
  df$e10<-substr(df$gridref,3,3)
  df$e1<-substr(df$gridref,4,4)
  df$n10<-substr(df$gridref,5,5)
  df$n1<-substr(df$gridref,6,6)
  df$tet<-'o'
  df$tet<-ifelse(df$e1==0 & df$n1==0,'A',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==1,'A',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==0,'A',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==1,'A',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==0,'F',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==1,'F',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==0,'F',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==1,'F',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==0,'K',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==1,'K',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==0,'K',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==1,'K',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==0,'Q',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==1,'Q',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==0,'Q',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==1,'Q',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==0,'V',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==1,'V',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==0,'V',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==1,'V',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==2,'B',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==3,'B',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==2,'B',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==3,'B',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==2,'G',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==3,'G',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==2,'G',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==3,'G',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==2,'L',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==3,'L',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==2,'L',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==3,'L',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==2,'R',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==3,'R',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==2,'R',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==3,'R',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==2,'W',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==3,'W',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==2,'W',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==3,'W',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==4,'C',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==5,'C',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==4,'C',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==5,'C',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==4,'H',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==5,'H',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==4,'H',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==5,'H',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==4,'M',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==5,'M',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==4,'M',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==5,'M',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==4,'S',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==5,'S',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==4,'S',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==5,'S',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==4,'X',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==5,'X',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==4,'X',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==5,'X',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==6,'D',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==7,'D',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==6,'D',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==7,'D',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==6,'I',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==7,'I',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==6,'I',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==7,'I',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==6,'N',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==7,'N',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==6,'N',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==7,'N',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==6,'T',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==7,'T',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==6,'T',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==7,'T',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==6,'Y',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==7,'Y',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==6,'Y',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==7,'Y',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==8,'E',df$tet)
  df$tet<-ifelse(df$e1==0 & df$n1==9,'E',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==8,'E',df$tet)
  df$tet<-ifelse(df$e1==1 & df$n1==9,'E',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==8,'J',df$tet)
  df$tet<-ifelse(df$e1==2 & df$n1==9,'J',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==8,'J',df$tet)
  df$tet<-ifelse(df$e1==3 & df$n1==9,'J',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==8,'P',df$tet)
  df$tet<-ifelse(df$e1==4 & df$n1==9,'P',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==8,'P',df$tet)
  df$tet<-ifelse(df$e1==5 & df$n1==9,'P',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==8,'U',df$tet)
  df$tet<-ifelse(df$e1==6 & df$n1==9,'U',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==8,'U',df$tet)
  df$tet<-ifelse(df$e1==7 & df$n1==9,'U',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==8,'Z',df$tet)
  df$tet<-ifelse(df$e1==8 & df$n1==9,'Z',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==8,'Z',df$tet)
  df$tet<-ifelse(df$e1==9 & df$n1==9,'Z',df$tet)
  df$tetrad_id<-paste(df$let,df$e10,df$n10,df$tet,sep='')
  df$let<-NULL
  df$tet<-NULL
  df$e10<-NULL
  df$e1<-NULL
  df$n10<-NULL
  df$n1<-NULL
  names(df)[invar.index]<-invar
  return(df)
}
