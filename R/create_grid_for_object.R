#' Create grid of squares for a spatial object
#'
#' @description Create a grid of a given resolution for a polygon. 
#' @details Rather than store shapefiles of grids of different sizes and projections, 
#' use this function to create a grid of a required resolution spanning a reigon of 
#' interest. Optionally limit the grid to cells overlapping sp_object.
#' @param sp_object = the polygon for which the grid is needed
#' @param grid_resolution = the resolution in metres of the desired grid
#' @param clip2object = bool, should squares with no overlap be removed?
#' @param region = One of GB (=Great Britain), I (=Ireland) or CH (=Channel Islands)
#' @return An sf object
#' 
#' @import sf
#' 
#' @export
create_grid_for_object <- function(sp_object, grid_resolution, clip2object = TRUE, region = NULL) {
  if(is.null(region)) stop('Region must be defined as one of GB, I or CH')
  if(!region %in% c('GB','I','CH')) stop('Region should be one of GB, I or CH')
  
  #set the target EPSG according to region
  if(region == 'GB') target_epsg <- 27700 #British national grid
  if(region == 'CH') target_epsg <- 23030 #ED50 / UTM zone 30N
  if(region == 'I') target_epsg <- 29903 #Irish grid
  
  
  #transform the object to target_epsg
  sp_object <- st_transform(sp_object, target_epsg)
  
  #get bounding box info for making grid
  bbox <- st_bbox(sp_object)
  
  #get extremes of grid according to the required grid resolution
  x1 <- floor(bbox[1]/grid_resolution)*grid_resolution
  y1 <- floor(bbox[2]/grid_resolution)*grid_resolution
  x2 <- ceiling(bbox[3]/grid_resolution)*grid_resolution
  y2 <- ceiling(bbox[4]/grid_resolution)*grid_resolution
  
  #make a polygon to cover the extent we need the grid to cover
  bb = st_sfc(st_polygon(list(rbind(c(x1,y1), c(x1,y2), c(x2,y2), c(x2,y1), c(x1,y1)))))
  
  #add crs so plots correctly
  bb <- st_sf(bb, crs = target_epsg)
  
  #create a grid using these dimensions
  grid <- st_make_grid(x = sp_object, #use sp_object to get the right projections etc
                       cellsize = grid_resolution, #size of the grid
                       offset = c(x1,y1), #ensures the grid starts on whole numbers
                       what = 'polygons',
                       square = TRUE)
  
  #convert to sf so can join
  grid <- st_as_sf(grid)
  #sometimes the geometry data is created but named x. If so, rename it
  if(attr(grid, "sf_column") != 'geometry') grid <- rename_geometry(grid)
  
  #optionally remove squares that do not overlap GB
  if(clip2object == TRUE) {
    #intersect with the object to remove any squares with no overlap
    sp_object$keep <- 1
    grid <- st_join(grid, sp_object, join = st_intersects)
    grid <- subset(grid, !is.na(keep))
    grid$keep <- NULL
  }
  
  # #get centroid of each square
  # cent <- st_centroid(grid)
  # 
  # #extract the geometry and convert to grid reference
  # cent_df <- as.data.frame(st_coordinates(cent), stringsAsFactors = FALSE)
  # squares <- coordinates_to_gridref(df = cent_df, invar_e = 'X', invar_n = 'Y', output_res = grid_resolution/1000)
  # squares$grid_ref <- as.character(squares$grid_ref)
  
  #calculate tenkm (briefly) to obtain segref and quadrefs
  # squares$tenkm <- substr(squares$grid_ref,1,4)
  # squares <- rescale_10km_to_20km(df = squares, invar = 'tenkm')
  # squares <- rescale_10km_to_50km(df = squares, invar = 'tenkm')
  # squares$tenkm <- NULL
  
  #bind the square info to the grid
  # grid <- sf:::cbind.sf(grid, squares)
  
  # names(grid)[which(names(grid) == 'grid_ref')] <- outvarname
  # grid$area_square <- NULL
  # grid$area_overlap <- NULL
  # grid$percentoverlap <- NULL
  
  return(grid)  
}
