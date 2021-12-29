#' Rename the geometry column of an sf object
#'
#' @description Rename geometry column of an sf object
#' @details Sometimes for unknown reason the geometry column of an sf object can be 
#' named 'x'instead of the expected 'geometry'. This function corrects this.
#' @param sf_object = the sf object to be corrected
#' @param name = the name to be used, defaulting to 'geometry'
#'
#' @import sf
#' 
#' @export
rename_geometry <- function(sf_object, name = 'geometry'){
  current = attr(sf_object, "sf_column")
  names(sf_object)[names(sf_object)==current] = name
  st_geometry(sf_object)=name
  return(sf_object)
}