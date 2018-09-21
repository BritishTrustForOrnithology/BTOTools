#' Load 1988-91 breeding atlas Timed Tetrad Visit data in unsummarised format
#'
#' @description
#' Loads raw TTV presence and count data for all TTVs during the 1988-91 breeding atlas. Note that
#' some tetrads were visited more than twice so these data cannot be used for fixed effort calculation
#' and instead the filtered data should be used (see load_safe_ttvs_1990). Note also that unlike
#' Bird Atlas 2007-11, not all species were counted - many were simply listed as present during TTVs,
#' hence the odd structure of 1s, 0s and counts which must be treated carefully so as not to incorrectly
#' infer presence or absence.
#'
#' @return
#' A dataframe with the following columns:
#' \item{tenkm}{10-km square surveyed}
#' \item{year}{year square surveyed}
#' \item{tetrad_num}{no. tetrads visited}
#' \item{tetrad_1v}{no. tetrads visited only once}
#' \item{A_visits}{number of visits to tetrad A}
#' \item{B_visits}{number of visits to tetrad B}
#' \item{...}{23 further variables for number of visits to remaining 23 tetrads}
#' \item{cbc_code}{2-letter species code}
#' \item{A}{data for species in tetrad A; see Details}
#' \item{B}{data for species in tetrad B; see Details}
#' \item{...}{23 further variables for species data in remaining 23 tetrads}
#' \item{total_tetrad_p}{num of tetrads a species was recorded in}
#' \item{total_count}{total count across all tetrads visited}
#' \item{cat}{breeding status of species based on these TTVs (S=seen, B=breeding); see Details}
#'
#' @details
#' IMPORTANT: these data are essentially a direct transcription of the paper forms, so the species
#' information in columns A to Z needs some careful interpretation. A zero in these columns denotes
#' a tick on the form, which meant a species was present. So a zero is NOT an absence. Any other number
#' in these columns should be for a count species (e.g. rare passerine, waterbird, colonial species) and
#' indicates the maximum count for the species across TTVs.
#'
#' Note also that the breeding evidence in the cat variable is based on observations made during the TTVs
#' only. Subsequent casual records will likely have increased the breeding evidence. This column isn't
#' really useful for the majority of analyses.
#'
#' @examples
#' df<-load_raw_ttvs_1990()
#'
#' @export
#'
load_raw_ttvs_1990<-function() {
  cat('WARNING: This is a big file (350k rows, 58 vars) and will be slow to load!\n')
  df<-read.fwf(paste0(.BTOarchive_path,'/birdatlas1988-91breed/data2km/gibbtetradsbysquare.txt'),
               header=F,
               widths=c(4,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,4,1),
               col.names = c("tenkm","year","tetrad_num","tetrad_1v", "A_visits","B_visits","C_visits","D_visits","E_visits","F_visits","G_visits",
                             "H_visits" , "I_visits","J_visits","K_visits","L_visits","M_visits","N_visits","P_visits","Q_visits","R_visits","S_visits","T_visits",
                             "U_visits" ,"V_visits","W_visits","X_visits","Y_visits","Z_visits", "cbc_code", "A","B","C","D","E","F","G","H","I","J","K","L","M",
                             "N", "P","Q","R","S","T","U","V","W","X","Y", "Z","total_tetrad_p","total_count","cat"),
               colClasses=c('character',rep('numeric',28),'character',rep('numeric', 27), 'character'),na.strings = ".")
  return(data)
}

head(df)
