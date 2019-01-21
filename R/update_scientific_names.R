#' Update species' scientific names
#'
#' @description
#' The function does not perform taxonomic changes such as splits and lumps. All it does is change 
#' names where there have been spelling changes (e.g. montanus to montana), or where genus names 
#' have changed (e.g. Parus to Periparus). It uses a lookup table of species synonyms which is 
#' available within the package and will be updated periodically (see See Also).
#'
#' @param df character The name of the dataset to be scanned for name updates
#' @param var character A quoted string. The name of the column in df to be scanned for name updates
#' @param report_changes logical Should the list of names changed be displayed on execution (default FALSE)
#' 
#' @return A dataframe.
#' 
#' @details If \code{report_changes=TRUE} the list of cases changed is printed to output, showing original
#' name (from), destination name (to) and the number of rows affected.
#'
#' @seealso \code{\link{sciname_synonyms}}
#'
#' @examples
#' old_records <- data.frame(sciname = c('Apus apus', 'Parus ater'), 
#'                           engname = c('Swift', 'Coal Tit'), 
#'                           stringsAsFactors = FALSE)
#' old_records_updated <- update_scientific_names(df = old_records, 
#'                                                var = 'sciname')
#'
#' @export
update_scientific_names <- function(df, var, report_changes=FALSE) {
  #errorcheck
  if(!(is.data.frame(df))) stop('df must be an existing dataframe')
  if(!var %in% names(df)) stop('var is not a column name in df')
  if(!is.logical(report_changes)) stop('report_changes must be TRUE or FALSE')
  
  #check which variable holds the scientific names to be scanned
  varn <- which(names(df) == var)
  
  #make a temporary copy 
  df$scinameworking <- df[,varn]
  
  #get the synonyms
  data("sciname_synonyms", envir = environment())
  #vectorise the list of correct scinames, setting row names as the old scinames
  temp <- as.vector(sciname_synonyms$current_sci_name)
  names(temp) <- sciname_synonyms$alternative_sci_name

  #merge new scinames into a new working column
  df$scinameworking2 <- temp[df$scinameworking]
  num_recs_in_df <- (nrow(df))
  num_recs_updated_in_df <- (nrow(df[!is.na(df$scinameworking2),]))
  cat(num_recs_updated_in_df, ' out of ', num_recs_in_df, ' records updated.')
  
  #optional reporting
  if(report_changes == TRUE) {
    optout <- subset(df, !is.na(scinameworking2))
    print(setNames(aggregate(data = optout, unit ~ scinameworking + scinameworking2, NROW), c('from', 'to', 'num_rows')))
  }
  
  #copy across the unchanged scinames
  df$scinameworking2 <- ifelse(is.na(df$scinameworking2), df$scinameworking, df$scinameworking2)
  #copy this new list of scinames overwriting the original column
  df[,varn] <- df$scinameworking2
  #clean up
  df$scinameworking <- NULL
  df$scinameworking2 <- NULL
  return(df)
}


