#' Ceiling to base
#' 
#' @description Calculate ceiling for certain base level, e.g. 72 to base 5 returns 75. 
#' Useful for creating sensible upper limit on graph axes.
#' 
#' @param x, number to be rounded using ceiling
#' @param base, number to use for rounding, e.g. to nearest 5
#' 
#' @examples
#' ceiling_to_base(72, 5)
#' 
#' @return numeric
#' 
#' @export
#' 
ceiling_to_base <- function(x,base){
  base*ceiling(x/base)
}
