#' Load open-access atlas data
#'
#' @description
#' Load summarised open-access distribution or change data from all bird atlases. These data have been 
#' summarised to up-scale rare breeding species.
#'
#' @param data_type Whether to return data for distribution (D) or distribution change (C)
#'
#' @return A dataframe with the following columns (some dependent on whether distribution or change data):
#' \item{speccode}{numeric species code}
#' \item{period}{character the atlass for which the distribution relates (Distribution data only)}
#' \item{interval}{character the atlass over which the change is calculated (Change data only)}
#' \item{season}{character B or W for Breeding or Winter season}
#' \item{grid}{character grid reference of the 10-km, 20-km or 50-km square}
#' \item{island}{character indicating whether the grid square is in Britain (B; includes Channel Isles) or Ireland (I)}
#' \item{resolution}{numeric the grid resolution at which data are supplied (10, 20, 50)}
#' \item{n_tenkms}{numeric the number of 10-km squares occupied in this grid square (Distribution data only)}
#' \item{n_tenkms_stable}{numeric the number of 10-km squares in this grid square that remained stable for the period in question (Change data only)}
#' \item{n_tenkms_gain}{numeric the number of 10-km squares in this grid square that gained the species for the period in question (Change data only)}
#' \item{n_tenkms_loss}{numeric the number of 10-km squares in this grid square that lost the species for the period in question (Change data only)}
#' \item{status}{character the occupancy status of the species in the grid square, e.g. Possible, Probable, COnfirmed (Distribution data only)}
#' \item{english_name}{character Species common name}
#' \item{scientific_name}{character Species scientific name}
#' \item{scientific_name}{character The taxonomic level of the entity (Species, Species aggregate or Variety)}
#'
#'
#' @examples
#' df1 <- load_open_access_atlas_data(data_type = 'D')
#' df2 <- load_open_access_atlas_data(data_type = 'C')
#'
#' @export
#'
load_open_access_atlas_data <- function(data_type) {
  #errorcheck
  if(missing(data_type)) stop("You must specify which data type you want.")
  if(!data_type %in% c('D', 'C')) stop("You must specify D (for distribution data) or C (for change data).")
  spinfo <- read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/open_data_species_lookup.csv'),
                   header=T, sep=",",
                   colClasses=c('numeric', 'character','character','character'))
  if(data_type == 'D') {
    df <- read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/open_data_distributions.csv'),
                   header=T, sep=",",
                   colClasses=c('character','character','numeric','character','character','numeric','numeric','character'))
  }
  if(data_type == 'C') {
    df <- read.table(paste0(.BTOarchive_path,'birdatlas2007-11/data/processed/open_data_distribution_changes.csv'),
                   header=T, sep=",",
                   colClasses=c('character','character','numeric','character','character','numeric','numeric','numeric','numeric'))
  }
  #add species info
  df <- merge(df, spinfo, by = 'speccode', all.x = TRUE)
  
  #check there are data returned
  if(nrow(df)<1) stop("Empty data frame; check path names")
  return(df)
}


