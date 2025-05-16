library(devtools)
library(usethis)
#' dataframe of species scientific name synonyms

#' read the old scientific names with their matched new names
sciname_synonyms <- read.csv('data-raw/sci_name_synonyms_IOC14_2.csv', 
                             colClasses = c('numeric','character','numeric', 'character', 'character','numeric', 'character','numeric'),
                             encoding = 'latin1')
names(sciname_synonyms) <- tolower(names(sciname_synonyms))
#filter to species only
sciname_synonyms <- subset(sciname_synonyms, alternative_rank_id == 100 & current_rank_id == 100)
sciname_synonyms$current_rank_id <- NULL
sciname_synonyms$current_rank_name <- NULL
sciname_synonyms$alternative_rank_id <- NULL
sciname_synonyms$alternative_rank_name <- NULL
sciname_synonyms$number_of_lists <- NULL
sciname_synonyms$sort_order_ioc <- NULL

#remove a couple of NA type values
sciname_synonyms <- subset(sciname_synonyms, master_taxon_id != 0)
sciname_synonyms <- subset(sciname_synonyms, alternative_sci_name != 'n/a')

#check for some old changes that appear to be missing; add if necessary
subset(sciname_synonyms, current_sci_name == 'Hydrobates leucorhous')
#sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(23, 'Hydrobates leucorhous', 'Oceanodroma leucorhoa')

subset(sciname_synonyms, current_sci_name == 'Poecile montanus')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(433, 'Parus montanus', 'Poecile montanus')

subset(sciname_synonyms, current_sci_name == 'Cyanistes caeruleus')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(436, 'Parus caeruleus', 'Cyanistes caeruleus')

subset(sciname_synonyms, current_sci_name == 'Saxicola rubicola')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(355, 'Saxicola torquatus', 'Saxicola rubicola')

subset(sciname_synonyms, current_sci_name == 'Luscinia svecica')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(349, 'Cyanecula svecica', 'Luscinia svecica')

#reorder
sciname_synonyms <- sciname_synonyms[order(sciname_synonyms$master_taxon_id),]
#output
usethis::use_data(sciname_synonyms, overwrite = TRUE)


#' dataframe of species names for all birds (globally) plus selected UK mammals, herps, inverts
global_species_lookup <- read.csv('data-raw/global_species_lookup_IOC14_2.csv', 
                                  colClasses = c('numeric', rep('character', 2), 'numeric', 'character','numeric', 'character', 'character', 'character', 'character', 'numeric', 'numeric','character' ), encoding = 'utf8', na.strings = "")
names(global_species_lookup) <- tolower(names(global_species_lookup))
names(global_species_lookup)[which(names(global_species_lookup)=='cbc_code')] <- 'code2ltr'
names(global_species_lookup)[which(names(global_species_lookup)=='five_letter')] <- 'code5ltr'
names(global_species_lookup)[which(names(global_species_lookup)=='euring_no')] <- 'euring'

#output
# ******************
usethis::use_data(global_species_lookup, overwrite = TRUE)

#' make coordinates datasets
centroids002 <- read.table('data-raw/002km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "character", "numeric", "numeric"))
centroids002$country <- NULL
centroids010 <- read.table('data-raw/010km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
centroids020 <- read.table('data-raw/020km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
names(centroids020)[1] <- 'segref'
centroids050 <- read.table('data-raw/050km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
centroids100 <- read.table('data-raw/100km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"), na.strings = '')
names(centroids100)[1] <- 'hundref'
#output
usethis::use_data(centroids002, overwrite = TRUE)
usethis::use_data(centroids010, overwrite = TRUE)
usethis::use_data(centroids020, overwrite = TRUE)
usethis::use_data(centroids050, overwrite = TRUE)
usethis::use_data(centroids100, overwrite = TRUE)


#' make land area dataset
landarea010 <- read.table('data-raw/BI010_area_above_low_tide_line.csv', sep = ",", header = T, colClasses = c("character", "numeric"))
usethis::use_data(landarea010, overwrite = TRUE)


#' lookup to update old speccode (from BirdAtlas era and before)
speccode_mapping <- read.table('data-raw/species_map_IOC73_to_IOC14_2.csv', 
                               sep = ',', 
                               header = TRUE, 
                               colClasses = c(rep('numeric',3), rep('character',3)), na.strings = c(''))
names(speccode_mapping) <- tolower(names(speccode_mapping))
speccode_mapping$mapping_id <- NULL
names(speccode_mapping)[1] <- 'old_species_code'
#per Andy M, need to filter as follows for B&I data
speccode_mapping <- subset(speccode_mapping, british_isles =='Y')
speccode_mapping$british_isles <- NULL
speccode_mapping$location_criteria <- NULL
speccode_mapping$country_list <- NULL
head(speccode_mapping)

#add a row for Red Grouse update
testrg <- subset(speccode_mapping, old_species_code==125)
if(nrow(testrg)==0) {
  cat('adding red grouse\n')
  addrg <- data.frame(old_species_code = 125, 
             new_species_code = 50734)
  
  speccode_mapping <- rbind(speccode_mapping, addrg)
  
}

#output
usethis::use_data(speccode_mapping, overwrite = TRUE)


#' lists of 1-km squares by country
squares_01km_england <- read.csv('data-raw/01km_england.csv')
squares_01km_england$country <- 'England'
usethis::use_data(squares_01km_england, overwrite = TRUE)

squares_01km_wales <- read.csv('data-raw/01km_wales.csv')
squares_01km_wales$country <- 'Wales'
usethis::use_data(squares_01km_wales, overwrite = TRUE)

squares_01km_scotland <- read.csv('data-raw/01km_scotland.csv')
squares_01km_scotland$country <- 'Scotland'
usethis::use_data(squares_01km_scotland, overwrite = TRUE)

squares_01km_northernireland <- read.csv('data-raw/01km_northernireland.csv')
squares_01km_northernireland$country <- 'Northern Ireland'
usethis::use_data(squares_01km_northernireland, overwrite = TRUE)

squares_01km_republicofireland <- read.csv('data-raw/01km_republicofireland.csv')
squares_01km_republicofireland$country <- 'Republic of Ireland'
usethis::use_data(squares_01km_republicofireland, overwrite = TRUE)

#' list of 10-km squares by dominant country
squares_10km_dominant_country <- read.csv('data-raw/10km_dominant_country.csv')
usethis::use_data(squares_10km_dominant_country, overwrite = TRUE)


