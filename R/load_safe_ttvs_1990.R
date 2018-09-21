#' Load "safe fixed effort" TTV data for 1988-91 breeding atlas
#'
#' @description
#' Although the full TTV dataset from 1988-91 can be loaded (see load_raw_ttvs_1990) initial checks
#' showed that some tetrads were visited more than twice so using those data could lead to incorrect
#' change measures owing to effort variation. The data loaded using this function have been filtered
#' according to two sets of rules to limit the data to a maximum of 2 visits per tetrad. It is recommended
#' that these data are used for any fixed effort analyses. Full details of the rules and how many records
#' are affected can be found in the documentation in the birdatlas1988-91breed Unixarchive folder.
#'
#' @param ruleset whether to load the data based on assumption rules 1 or 2 (see Details)
#'
#' @details
#' Two different versions of the data have been created using different assumptions to decide which
#' duplicated visit to use vs remove. Ruleset 1 assumes that TTVs were repeated where the original visits
#' were substandard. On this basis, where tetrads received excess visits, the TTVs with the lower richness
#' were removed (checks showed many were woefully low so this is probably not simply picking the better of
#' two good visits). Ruleset 2 makes no assumptions about data quality and simply takes the first two visits,
#' wither based on the year of coverage, or order in the data file.
#'
#' @return A dataframe with the following columns:
#' \item{tenkm}{10-km grid reference}
#' \item{tetlet}{tetrad letter}
#' \item{n.visits}{number of visits}
#' \item{cbc_code}{2-letter species code}
#' \item{present}{species was recorded (1/NA)}
#' \item{count}{maximum number of individuals for count species (numeric/NA)}
#'
#' @examples
#' df<-load_safe_ttvs_1990(ruleset=1)
#'
#' @export
load_safe_ttvs_1990<-function(ruleset) {
  df<-read.table(paste0(.BTOarchive_path,'birdatlas1988-91breed/data2km_safe/excess_coverage_removed_ruleset',ruleset,'.csv'),
                 header=T,sep=",",
                 colClasses=c('character','character','numeric','character','numeric','numeric' ))
  names(df)[2]<-'tetlet'
  return(df)
}



