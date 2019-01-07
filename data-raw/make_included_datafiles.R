library(devtools)
#' dataframe of species scientific name synonyms

#' read the old scientific names with their matched new names
sciname_synonyms <- read.csv('data-raw/sci_name_synonyms.csv', colClasses = c('numeric','character','character','numeric'))
names(sciname_synonyms) <- tolower(names(sciname_synonyms))

#remove a couple of NA type values
sciname_synonyms <- subset(sciname_synonyms, master_taxon_id != 0)
sciname_synonyms <- subset(sciname_synonyms, alternative_sci_name != 'n/a')

#output
use_data(sciname_synonyms)


#' dataframe of species names for all birds (globally) plus selected UK mammals, herps, inverts
global_species_lookup <- read.csv('data-raw/global_species_lookup.csv', 
                                  colClasses = c('numeric', rep('character', 2), 'numeric', 'character','numeric', 'character', 'character' ))
names(global_species_lookup) <- tolower(names(global_species_lookup))
# head(global_species_lookup)
# str(global_species_lookup)

#infill NA values for cbc_code
global_species_lookup$cbc_code <- ifelse(global_species_lookup$cbc_code == '', NA, global_species_lookup$cbc_code)

#output
use_data(global_species_lookup, overwrite = TRUE)
