#' Handle numbers, text and singular or plural verbs
#' @description Convert numbers to text and pluralise units on demand. 
#' @details This function is designed for use in RMarkdown notebooks and handles conversion 
#' of numbers below 10 to words. It also handles whether numbers should be presented as 
#' words at the start of a sentence   function also capitalise if needed for start of 
#' sentence. And lastly it handles display of the verb or units as singular or plural.
#' 
#' @param x = the number to be formated
#' @param unit = text string of the singular form of unit, e.g. '10-km square'
#' @param st = whether the string will be used at the start of a sentence. Determines conversion of number and capitalisation
#'
#' @return = a string containing the count as a numeral or text and a verb or unit, pluralised as required.
#'
#' @examples 
#' num2text(x = 7, unit = 'species', st = FALSE) 
#' num2text(x = 81, unit = 'tetrad', st = TRUE) 
#' num2text(x = 1547, unit = '1-km square', st = FALSE) 
#' 
#' @import english
#' @import textclean
#' 
#' @export
#' 
num2text <- function(x, unit=NULL, st = FALSE ) {
  #If the start of a sentence, convert to words regardless of size of number 
  if(st == TRUE) {
    if(x == 0) string <- paste(english::Words(x), textclean::make_plural(unit))
    if(x == 1) string <- paste(english::Words(x), unit)
    if(x > 1) string <- paste(english::Words(x), textclean::make_plural(unit))
  }
  #If not start of sentence, convert to words only if <10 
  if(st == FALSE) {
    if(x == 1) string <- paste(english::words(x), unit)
    if(x == 0 | (x >1 & x < 10)) string <- paste(english::words(x), textclean::make_plural(unit))
    if(x >= 10) string <- paste(x, textclean::make_plural(unit))
  }
  #remove any leading or trailing white space introduced here
  string <- trimws(string)
  return(string)
}