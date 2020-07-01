#' Update old species' numeric codes
#'
#' @description
#' The function does not perform taxonomic changes such as splits and lumps. All it does is change 
#' the species code used in old archived files to the newer version used everywhere else. This then 
#' allows use of the global_species_dictionary. It uses a lookup table of species code mappings 
#' which is available within the package.
#'
#' @param df character The name of the dataset in which speccodes will be updated
#' @param var character A quoted string. The name of the column containing species codes to be updated
#' @param report_changes logical Should the list of changes be displayed on execution (default FALSE)
#' 
#' @return A dataframe.
#' 
#' @details If \code{report_changes=TRUE} the list of cases changed is printed to output, showing original
#' name (from), destination name (to) and the number of rows affected.
#'
#' @seealso \code{\link{speccode_mapping}}
#'
#' @examples
#' old_records <- data.frame(speccode = c(36, 153),
#'                           engname = c('Cattle Egret', 'Black-winged Stilt'),
#'                           stringsAsFactors = FALSE)
#' old_records_updated <- update_speccodes(df = old_records,
#'                                              var = 'speccode')
#'
#' @export
update_speccodes <- function(df, var, report_changes=FALSE) {
  #errorcheck
  if(!(is.data.frame(df))) stop('df must be an existing dataframe')
  if(!var %in% names(df)) stop('var is not a column name in df')
  if(!is.logical(report_changes)) stop('report_changes must be TRUE or FALSE')
  
  #check which variable holds the speccodes to be scanned
  varn <- which(names(df) == var)
  
  #make a temporary copy 
  df$speccodeworking <- df[,varn]
  
  #get the synonyms
  data("speccode_mapping", envir = environment())
  #load('data/speccode_mapping.rda')

  #merge new speccodes into a new working column
  df <- merge(df, speccode_mapping, by.x = 'speccodeworking', by.y = 'old_species_code', all.x = TRUE)
  num_recs_in_df <- (nrow(df))
  num_recs_updated_in_df <- (nrow(df[!is.na(df$new_species_code),]))
  cat(num_recs_updated_in_df, ' out of ', num_recs_in_df, ' records updated.\n\n')
  
  #optional reporting
  if(report_changes == TRUE) {
    optout <- subset(df, !is.na(new_species_code))
    optout$unit <- 1
    print(setNames(aggregate(data = optout, unit ~ speccodeworking + new_species_code, NROW), c('from', 'to', 'num_rows')))
  }
  
  #copy across the unchanged scinames
  df$new_species_code <- ifelse(is.na(df$new_species_code), df$speccodeworking, df$new_species_code)
  #recheck which variable holds the speccodes to be scanned - can move during merge
  varn <- which(names(df) == var)
  #copy this new list of speccode overwriting the original column
  df[,varn] <- df$new_species_code
  #clean up
  df$speccodeworking <- NULL
  df$new_species_code <- NULL
  return(df)
}


