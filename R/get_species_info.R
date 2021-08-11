#' Get species names and codes for a taxonomic entity
#'
#' @description
#' Given a species name, scientific name, or species code, return all other names and codes for this entity.
#'
#' @param field str = which field to search on, one of: id, name, sciname, 2ltr, 5ltr or euring
#' @param value str or numeric = the value to search with, e.g. 10
#' 
#' @return A one-row dataframe.
#' 
#' @examples
#' get_species_info('id', 18)
#' get_species_info('name', 'Blue Tit')
#' get_species_info('sciname', 'Emberiza calandra')
#' get_species_info('2ltr', 'W.')
#' get_species_info('5ltr', 'JAY..')
#' get_species_info('euring', 11870)
#' 
#' @export
get_species_info <- function(field, value) {
  data("global_species_lookup", package = 'BTOTools')
  #check field is a valid vield name
  valid_fields <- c('euring', 'name', 'sciname', 'id', '2ltr', '5ltr')
  if(!field %in% valid_fields) stop('field must be one of: name, sciname, id, 2ltr, 5ltr, euring')

  #search on master_taxon_id  
  if(field == 'id') {
    if(!is.numeric(value)) stop('For search on taxon id, value must be numeric')
    temp <- subset(global_species_lookup, master_taxon_id == value)
  }

  #search on species name  
  if(field == 'name') {
    if(!is.character(value)) stop('For search on species name, value must be character')
    temp <- subset(global_species_lookup, english_name == value)
  }

  #search on scientific name  
  if(field == 'sciname') {
    if(!is.character(value)) stop('For search on scientific name, value must be character')
    temp <- subset(global_species_lookup, scientific_name == value)
  }
  
  #search on 2-letter code name  
  if(field == '2ltr') {
    if(!is.character(value)) stop('For search on 2-letter code, value must be character')
    if(nchar(value)!=2) stop('For search on 2-letter code, value must be a 2 character string. Don\'t forget trailing dot for single letter codes')
    temp <- subset(global_species_lookup, code2ltr == value)
  }
  
  #search on 5-letter code name  
  if(field == '5ltr') {
    if(!is.character(value)) stop('For search on 5-letter code, value must be character')
    if(nchar(value) != 5) stop('For search on 5-letter code, value must be a 5 character string. Remember trailing dots for short names, e.g. "WREN."')
    temp <- subset(global_species_lookup, code5ltr == value)
  }
  
  #search on Euring code  
  if(field == 'euring') {
    if(!is.numeric(value)) stop('For search on Euring code, value must be numeric')
    temp <- subset(global_species_lookup, euring == value)
  }
  

  #check something has been returned
  if(nrow(temp) == 0) warning(paste('No species found with',field,'value of',value))
  
  #remove some redundant fields
  temp$taxon_rank_id <- NULL
  temp$taxon_rank_name <- NULL
  temp$alt_int_name <- NULL
  temp$cbc_code <- NULL
  
  #return the result
  return(temp)  
}




