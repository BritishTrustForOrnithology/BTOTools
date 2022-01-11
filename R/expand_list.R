#' Format a list of words with commas and 'and'
#' 
#' @description Take a list and pad it out with commas and 'and'.
#' @details For use in RMarkdown documents to convert lists of species, 
#' for example, into grammatically correct sentences. 
#' 
#' @param inval a vector of strings to be expanded
#' 
#' @examples
#' x <- c('Robin', 'Dunnock', 'Wren')
#' expand_list(inval = x)
#' 
#' @export
#' 
expand_list <- function(inval) {
  if(length(inval) == 1) {outval <- inval}
  if(length(inval) == 2) {outval <- paste0(inval, collapse = ' and ')}
  if(length(inval) > 2) {
    seps <- c(rep(', ', length(inval)-2), ' and ', '')
    outval <- paste0(inval, seps, collapse = '')
  }
  return(outval)
}