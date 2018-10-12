#' Load centroid coordinates for squares of various sizes
#'
#' @description
#' Chose to load coordinates for tetrads, 10-km, 20-km, 50-km or 100-km squares. Note that coordinates
#' for the Channel Isles have been moved to where they were mapped in the Atlas book.
#'
#' @param res numeric resolution of the coordinates required: 2, 10, 20, 50 or 100
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km square, or tetrad_id, segref, quadref, hundref depending on chosen grid resolution}
#' \item{easting}{British National grid easting of square centroid}
#' \item{northing}{British National grid northing of square centroid}
#'
#' @examples
#' df<-load_coords(res=10)
#'
#' @export
load_coords<-function(res) {
  #errorcheck
  if(!res %in% c(2, 10, 20, 50, 100)) stop('Resolution must be one of: 2, 10, 20, 50, 100')
  
  #convert input resolution to text for file name
  res<-ifelse(res==2,'002',res)
  res<-ifelse(res==10,'010',res)
  res<-ifelse(res==20,'020',res)
  res<-ifelse(res==50,'050',res)
  res<-ifelse(res==100,'100',res)
  #get the relevant file
  if(res!='002') {
    df<-read.table(paste(.BTOarchive_path,'birdatlas2007-11/data/lookups/sas-staticdata/',res,'km_xy_coords.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric','numeric'))
  }
  if(res=='002') {
    df<-read.table(paste(.BTOarchive_path,'birdatlas2007-11/data/lookups/sas-staticdata/',res,'km_xy_coords.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','character','numeric','numeric'))
    df<-df[,c(1,3,4)]
  }
  df$easting<-round(df$easting,digits=0)
  df$northing<-round(df$northing,digits=0)
  names(df)<-tolower(names(df))
  return(df)
}

