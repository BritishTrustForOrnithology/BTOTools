library(devtools)
#' dataframe of species scientific name synonyms

#' read the old scientific names with their matched new names
sciname_synonyms <- read.csv('data-raw/sci_name_synonyms.csv', colClasses = c('numeric','character','character','numeric'), encoding = 'latin1')
names(sciname_synonyms) <- tolower(names(sciname_synonyms))
sciname_synonyms$sort_order_ioc <- NULL

#remove a couple of NA type values
sciname_synonyms <- subset(sciname_synonyms, master_taxon_id != 0)
sciname_synonyms <- subset(sciname_synonyms, alternative_sci_name != 'n/a')

#add a couple of manual entries (use list instead of c to prevent class changes)
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(23, 'Hydrobates leucorhous', 'Oceanodroma leucorhoa')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(433, 'Parus montanus', 'Poecile montanus')
sciname_synonyms[NROW(sciname_synonyms) + 1,] <- list(436, 'Parus caeruleus', 'Cyanistes caeruleus')
sciname_synonyms <- sciname_synonyms[order(sciname_synonyms$master_taxon_id),]

#output
use_data(sciname_synonyms, overwrite = TRUE)


#' dataframe of species names for all birds (globally) plus selected UK mammals, herps, inverts
global_species_lookup <- read.csv('data-raw/global_species_lookup.csv', 
                                  colClasses = c('numeric', rep('character', 2), 'numeric', 'character','numeric', 'character', 'character' ), encoding = 'latin1')
names(global_species_lookup) <- tolower(names(global_species_lookup))
# head(global_species_lookup)
# str(global_species_lookup)
#check the encoding of an accented character
#subset(global_species_lookup, master_taxon_id == 405)

#infill NA values for cbc_code
global_species_lookup$cbc_code <- ifelse(global_species_lookup$cbc_code == '', NA, global_species_lookup$cbc_code)

#output
use_data(global_species_lookup, overwrite = TRUE)

#' make coordinates datasets
centroids002 <- read.table('data-raw/002km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "character", "numeric", "numeric"))
centroids002$country <- NULL
centroids010 <- read.table('data-raw/010km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
centroids020 <- read.table('data-raw/020km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
names(centroids020)[1] <- 'segref'
centroids050 <- read.table('data-raw/050km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
centroids100 <- read.table('data-raw/100km_xy_coords.csv', sep = ",", header = T, colClasses = c("character", "numeric", "numeric"))
names(centroids100)[1] <- 'hundref'
#output
use_data(centroids002, overwrite = TRUE)
use_data(centroids010, overwrite = TRUE)
use_data(centroids020, overwrite = TRUE)
use_data(centroids050, overwrite = TRUE)
use_data(centroids100, overwrite = TRUE)


#' make land area dataset
landarea010 <- read.table('data-raw/BI010_area_above_low_tide_line.csv', sep = ",", header = T, colClasses = c("character", "numeric"))
use_data(landarea010, overwrite = TRUE)

