#' Load land area of 10-km square
#'
#' @description
#' Land area of the 10-km square. Two versions are available. One (type=BIlow) is based on low tide
#' mark and is available for Britain and Ireland. The other (type=GBhigh) is based on high tide mark
#' and is only available for GB.
#'
#' @param type Which version of areas to load: Britain & Ireland low tide (default) or GB high tide
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km square}
#' \item{area}{area in square km}
#'
#' @examples
#' df<-load_area_10km(type=BIlow)
#'
#' @export
load_area_10km<-function(type='BIlow') {
  if(type=='BIlow') {
    df<-read.table(paste(.BTOarchive_path,'birdatlas2007-11/data/lookups/sas-staticdata/BI010_area_above_low_tide_line.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric'))
  }
  if(type=='GBhigh') {
    df<-read.table(paste(.BTOarchive_path,'birdatlas2007-11/data/lookups/sas-staticdata/GB010_area_above_mean_high_water.csv',sep=''),
                     sep=',',
                     header=T,
                     colClasses=c('character','numeric'))
  }
  return(df)
}

