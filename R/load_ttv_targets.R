#' Load target number of tetrads for 2008-11 TTV coverage and analysis
#'
#' @description
#' In each 10-km square a (minimum) target of 8 tetrads should have been surveyed in each 10-km square
#' unless fewer than 8 tetrads had their centre on land, in which case all available tetrads had to be
#' surveyed. Additionally, in Ireland a chequerboard was applied such that only every other 10-km square
#' had to be covered for TTVs. This function loads the relevant 10-km squares and the minimum number of
#' tetrads that were supposed to have been covered. These figures were fine for planned coverage but for
#' analysis they lead to biased coverage in coastal areas. So an adjusted target was created based on
#' proportional land area.
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{country}{B = Britain, I = Ireland}
#' \item{rule8}{target number of tetrads for coverage}
#' \item{rule8adj}{adjusted target based on land area for analysis}
#'
#' @examples
#' df<-load_ttv_targets()
#'
#' @export
#'
load_ttv_targets<-function() {
  df<-read.csv(paste(.BTOarchive_path,'birdatlas2007-11/data/lookups/rule8target.csv', sep='')
                 ,header=T
                 ,colClasses=c('character','character','numeric','numeric')
  )
  df<-df[,c(2,1,3,4)]
  return(df)
}
