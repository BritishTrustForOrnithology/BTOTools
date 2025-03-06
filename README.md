# BTOTools
A package for manipulating BTO datasets

Remember that if you want to install this directly from Github using devtools::install_github you will first need to create a Personal Access Token. Basic instructions are:

* Go to https://github.com/settings/tokens
* Generate a new token, call it devtools and tick repo to define its scope
* Before navigating away be sure to copy the token as it will not be shown again (if you forget you’ll need to delete this one and make a new one)
* Now go to R and add the token to the .Renviron file. The easiest way is to use usethis::edit_r_environ() (you may need to install the usethis package)
* Using this command opens the .Renviron file for editing. Add the following line, substituting your token code, followed by two carriage returns. Then save the file, close it and restart R:
GITHUB_PAT=blahblahblahblahblahblahblahblahblahblahblahblah

Once you’ve done this you can install the package directly:
devtools::install_github('BritishTrustForOrnithology/BTOTools', build_vignettes = TRUE)

# Versions

## Update 06/03/2025 (version 0.1.6)

A minor update to upgrade the inbuilt species dictionary to IOC 14.2. Note the global_species_lookup now also includes two additional columns: i) pipeline_code contains the 6-letter codes used for bats and other ultrasonic vocalising species in the Acoustic Pipeline and ii) parent_species_mti which contains the 'parent' master_taxon_id of a taxon. This is most useful when 'rolling up' subspecies records to their parent species.

## Update 18/01/2024 (version 0.1.5)

A minor update to add lists of 1-km squares for each country, and a list of 10-km squares assigned to dominant country based on land area. data can be accessed by e.g. data('squares_01km_wales')

## Update 31/10/2023 (version 0.1.4)

A minor update to replace the embedded global_species_lookup and sciname_synonyms datasets to include the latest IOC version 13.2 species names and order.

## Update 10/02/2022

A minor update to replace the embedded global_species_lookup and sciname_synonyms datasets to include the latest IOC version 12.1 species names and order. 

## Update 11/01/2022

Three improvements in this update:

* coordinates_to_gridref() and gridref_to_coordinates now work in Ireland and Channel Islands as well as in GB. For coordinates_to_gridref it is now necessary to add a *region* parameter to the function call to specify on which grid system (GB, I or CH) the coordinates originate. 
* create_grid_for_object - this is a (beta) function to create a grid of squares of desired resolution for a spatial object. e.g. if you have a county and want all 1-km squares overlapping the county, this function aims to create such a grid

* rename_geometry - a helper function needed inside create_grid_for_object to deal with instances where the geometry field of a simple features object is misnamed

## Update 11/08/2021

A minor update to replace the embedded global_species_lookup and sciname_synonyms datasets to include the latest IOC version 11.2 species names and order. Also includes a new function (get_species_info()) to query species information given a name or code.

## Update 01/07/2020

Added new dataset (speccode_mapping) and associated function (update_speccodes()) which provide translation of legacy numeric species codes (e.g. as used in archived files of all atlases) into currently used species codes (master_taxon_ids). When using any old archive data files with numeric species codes, you are advised to run update_speccodes() before merging with the data(global_species_lookup).

## Update 10/06/2020

A minor update to include Euring code in the embedded global_species_lookup.

## Update 08/06/2020

A minor update to replace the embedded global_species_lookup and sciname_synonyms datasets to include the latest IOC version 10.1 species names and order. Also added is 5-letter codes as used in demography (ringing and NRS). Consequently I have included new variables code2ltr and code5ltr. cbc_code is retained for backwards compatibility but recommend using code2ltr hereafter. 




