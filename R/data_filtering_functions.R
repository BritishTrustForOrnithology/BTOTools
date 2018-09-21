#data filtering functions

#' Remove colonial seabirds from a dataframe
#'
#' @description
#' Remove colonial seabirds from a dataframe using either speccode or cbc_code variable.
#'
#' @param df name of input dataframe
#' @param var2use whether to use speccode or cbc_code to filter species
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' distrib2010.noseabirds<-exclude_seabirds(distrib2010,'speccode')
#'
#' @export
#'
exclude_seabirds<-function(df,var2use) {
  if(var2use=='speccode') df<-subset(df, !speccode %in% c(12,18,22,23,25,26,27,222,224,242,232,226,235,229,234,236,237,1282,1283,238,239,240,256,258,248,250,251,249,260,262,264,266))
  if(var2use=='cbc_code') df<-subset(df, !cbc_code %in% c('F.','MX','TM','TL','GX','CA','SA','AC','NX','KI','BH','MU','CM','LU','IN','LB','HG','YG','YC','IG','GZ','GB','AF','BJ','TE','CN','AE','RS','GU','RA','TY','PU','F_'))
  return(df)
}


#' Remove subspecies, hybrids and domestic forms from a dataframe
#'
#' @description
#' Remove subspecies, hybrids and domestic forms from a dataframe. As some of these do not have a
#' cbc_code this can only be done using numeric speccode variable.
#'
#' @param df name of input dataframe
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' distrib2010.truespecsonly<-exclude_hybrids(distrib2010)
#'
#' @export
#'
exclude_hybrids<-function(df) {
  df<-subset(df, !speccode %in% c(873,874,875,876,911,1021,1053,1152,1165,1247,1278,1279))
  return(df)
}



#' Limit a dataframe to the chequerboard in Ireland
#'
#' @description
#' For some analyses it may be necessary to remove the 10-km squares in Ireland that were not on the
#' chequerboard and hence did not achieve the minimum TTV coverage.
#'
#' @param df name of input dataframe
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' df<-limit2chequerboard_ireland(irish.ttvs)
#'
#' @export
#'
limit2chequerboard_ireland<-function(df) {
  df$en<-as.numeric(substr(df$tenkm,3,3))+as.numeric(substr(df$tenkm,4,4))
  df$en<-ifelse(substr(df$tenkm,1,1)=='I',df$en,NA)
  df<-subset(df, !en %in% c(0,2,4,6,8,10,12,14,16,18) )
  df$en<-NULL
  return(df)
}

#' Limit a dataframe to the chequerboard in Britain
#'
#' @description
#' For some analyses it may be necessary to impose the chequerboard approach onto Britain (even though
#' all 10-km squares were actually targetted there).
#'
#' @param df name of input dataframe
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' df<-limit2chequerboard_britain(irish.ttvs)
#'
#' @export
#'
limit2chequerboard_britain<-function(df) {
  df$en<-as.numeric(substr(df$tenkm,3,3))+as.numeric(substr(df$tenkm,4,4))
  df$en<-ifelse(substr(df$tenkm,1,1)=='I',df$en,NA)
  df<-subset(df, !en %in% c(0,2,4,6,8,10,12,14,16,18) )
  df$en<-NULL
  return(df)
}



#' Remove data for the Channel Islands from a dataframe
#'
#' @description
#' Remove all records for the Channel Isles from a dataframe
#'
#' @param df name of input dataframe
#' @param var2use name of grid reference style variable on which to remove records
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' df<-exclude_channel_isles(distrib2010,'tenkm')
#'
#' @export
#'
exclude_channel_isles<-function(df,var2use='tenkm') {
  v<-which(names(df)==var2use)
  df<-subset(df,substr(df[,v],1,1)!='W')
  return(df)
}


#' Remove data for Ireland from a dataframe
#'
#' @description
#' Remove all records for Ireland from a dataframe
#'
#' @param df name of input dataframe
#' @param var2use name of grid reference style variable on which to remove records
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' df<-exclude_ireland(distrib2010,'tenkm')
#'
#' @export
#'
exclude_ireland<-function(df,var2use='tenkm') {
  v<-which(names(df)==var2use)
  df<-subset(df,substr(df[,v],1,1)!='I')
  return(df)
}



#' Remove data for Britain from a dataframe
#'
#' @description
#' Remove all records for Britain from a dataframe
#'
#' @param df name of input dataframe
#' @param var2use name of grid reference style variable on which to remove records
#'
#' @return A dataframe in same format as the input dataset.
#'
#' @examples
#' df<-exclude_britain(distrib2010,'tenkm')
#'
#' @export
#'
exclude_britain<-function(df,var2use='tenkm') {
  v<-which(names(df)==var2use)
  df<-subset(df,!(substr(df[,v],1,1) %in% c('H','N','S','T')) )
  return(df)
}



#' Remove suspect breeding records from 1988-91 distribution data
#'
#' @description
#' Differences in breeding evidence criteria mean that some apparent "breeding" records in the
#' 1988-91 atlas most likely represent migrants or non-breeders. These complicate range change
#' comparisons and in some cases it may be helpful to remove them. They are difficult to identify
#' but this function removes some obvious ones (e.g. extralimital passage waders). This function
#' requires species to be identified by numeric speccode variable and squares by character tenkm variable.
#'
#' @param df name of input dataframe
#'
#' @examples
#' distrib1990.clean<-delete_1990_extralimitals(distrib1990)
#'
#' @export
#'
delete_1990_extralimitals<-function(df) {
  #these are 100-km squares where these species are unlikely to have bred or there is no
  #suitable habitat
  eider<-c('NZ','TA','TF','TM','TR','SU','SX','SS') #81
  cscoter<-c('NT','NU','TA','TF','TR','SU','SX','SS') #86
  goldeneye<-c('NR','NS','NT','NU','NX','NY','NZ','SC','SD','SE','TA','SH','SJ','SK','TG','SO','SU','SY') # 91
  osprey<-c('NY','NZ','SK','TF','SP','TL','TQ') #114
  goldenplover<-c('TL') #167
  dunlin<-c('TA','TF','TG','TL','TQ','SU','SZ','SV','ST') #186
  commonsandpiper<-c('TA','TF','TG','SP','TL','TM','SU','TQ','TR','SZ') #215
  piedfly<-c('TG','TM') #429
  df<-subset(df,!(speccode==81 & substr(tenkm,1,2) %in% eider))
  df<-subset(df,!(speccode==86 & substr(tenkm,1,2) %in% cscoter))
  df<-subset(df,!(speccode==91 & substr(tenkm,1,2) %in% goldeneye))
  df<-subset(df,!(speccode==113 & substr(tenkm,1,2) %in% osprey) )
  df<-subset(df,!(speccode==167 & substr(tenkm,1,2) %in% goldenplover))
  df<-subset(df,!(speccode==186 & substr(tenkm,1,2) %in% dunlin) )
  df<-subset(df,!(speccode==215 & substr(tenkm,1,2) %in% commonsandpiper))
  df<-subset(df,!(speccode==429 & substr(tenkm,1,2) %in% piedfly) )
  return(df)
}
