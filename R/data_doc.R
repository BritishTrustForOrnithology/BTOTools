#' Species dictionary lookup
#'
#' Common names, scientific names, two-letter codes, five-letter codes, sort order and taxonomic rank for birds, 
#' mammals and various inverts. Now with 5-letter codes for all European breeding birds. Bird names as used in 
#' IOC version 12.2, last updated October 2023.
#'
#' @name global_species_lookup
#' @docType data
#' @format A data.frame with 34290 rows and 11 columns
#' \describe{
#'   \item{numeric}{master_taxon_id = Numeric taxon key code}
#'   \item{character}{scientific_name = Species' scientific name}
#'   \item{character}{english_name = Species' common name as in common usage in UK}
#'   \item{numeric}{taxon_rank_id = Taxonomic rank identification (numeric) of entity}
#'   \item{character}{taxon_rank_name = Taxonomic rank of entity}
#'   \item{numeric}{sort_order = Sort order under IOC}
#'   \item{character}{alt_int_name = Species' common name as used internationally (e.g. with Eurasian prefix)}
#'   \item{character}{code2ltr = two-letter code if exists. To be used in preference to cbc_code}
#'   \item{character}{code5ltr = five-letter code, if exists}
#'   \item{numeric}{euring = Euring number if exists}
#'   \item{character}{taxa} = Common name of species group (e.g. Birds, Butterflies, Reptiles)
#' }
#' @references IOC
#' @keywords data
NULL

#' Species' scientific name synonyms
#'
#' A list of previously used scientific names and their currently used synonym, to assist in updating taxonomy 
#' of old data files. Bird names as used in IOC version IOC version 13.2, last updated October 2023.
#'
#' @name sciname_synonyms
#' @docType data
#' @format A data.frame with 2337 rows and 4 columns
#' \describe{
#'   \item{numeric}{Numeric taxon key code}
#'   \item{character}{Formerly used scientific name}
#'   \item{character}{Currently used scientific name}
#' }
#' @keywords data
NULL

#' Centroid coordinates of 2-km squares (tetrads)
#'
#' A list of coordinates on British National Grid of the centroid of all British and Irish tetrads. Channel Islands squares are included 
#' but note the location has been shifted as used in Bird Atlas maps.
#'
#' @name centroids002
#' @docType data
#' @format A data.frame with 84969 rows and 3 columns
#' \describe{
#'   \item{character}{tetrad_id (e.g. TL88A)}
#'   \item{numeric}{easting on British National Grid (metres)}
#'   \item{numeric}{northing on British National Grid (metres)}
#' }
#' @keywords data
NULL

#' Centroid coordinates of 10-km squares (hectads)
#'
#' A list of coordinates on British National Grid of the centroid of all British and Irish 10-km squares. Channel Islands squares are included 
#' but note the location has been shifted as used in Bird Atlas maps.
#'
#' @name centroids010
#' @docType data
#' @format A data.frame with 5353 rows and 3 columns
#' \describe{
#'   \item{character}{tenkm (e.g. TL88)}
#'   \item{numeric}{easting on British National Grid (metres)}
#'   \item{numeric}{northing on British National Grid (metres)}
#' }
#' @keywords data
NULL

#' Centroid coordinates of 20-km squares
#'
#' A list of coordinates on British National Grid of the centroid of all British and Irish 20-km squares. Channel Islands squares are included 
#' but note the location has been shifted as used in Bird Atlas maps.
#'
#' @name centroids020
#' @docType data
#' @format A data.frame with 1386 rows and 3 columns
#' \describe{
#'   \item{character}{segref (e.g. TL_A) where notation is as described in Bird Atlas p35}
#'   \item{numeric}{easting on British National Grid (metres)}
#'   \item{numeric}{northing on British National Grid (metres)}
#' }
#' @keywords data
NULL

#' Centroid coordinates of 50-km squares
#'
#' A list of coordinates on British National Grid of the centroid of all British and Irish 50-km squares. Channel Islands squares are included 
#' but note the location has been shifted as used in Bird Atlas maps.
#'
#' @name centroids050
#' @docType data
#' @format A data.frame with 224 rows and 3 columns
#' \describe{
#'   \item{character}{quadref (e.g. TLSW) where notation is as described in Bird Atlas p35}
#'   \item{numeric}{easting on British National Grid (metres)}
#'   \item{numeric}{northing on British National Grid (metres)}
#' }
#' @keywords data
NULL

#' Centroid coordinates of 100-km squares
#'
#' A list of coordinates on British National Grid of the centroid of all British and Irish 100-km squares. Channel Islands squares are included 
#' but note the location has been shifted as used in Bird Atlas maps.
#'
#' @name centroids100
#' @docType data
#' @format A data.frame with 74 rows and 3 columns
#' \describe{
#'   \item{character}{hundref (e.g. TL) where notation is as described in Bird Atlas p35}
#'   \item{numeric}{easting on British National Grid (metres)}
#'   \item{numeric}{northing on British National Grid (metres)}
#' }
#' @keywords data
NULL

#' Land area of each 10-km squares
#'
#' The area (in sqr km) of each 10-km square in Britain, Ireland and the Channel Islands, based on low tide line.
#'
#' @name landarea010
#' @docType data
#' @format A data.frame with 3894 rows and 2 columns
#' \describe{
#'   \item{character}{tenkm (e.g. TL88)}
#'   \item{numeric}{area (square km)}
#' }
#' @keywords data
NULL

#' Species code mapping
#'
#' Following the move to use IOC world bird names, some species codes had to be updated in the Oracle system. 
#' However, older archived files (e.g. those for BirdAtlas 2007-11 and earlier ) still have the old species code 
#' encoding. For example Cattle Egret will be represented in old files as speccode = 35 whereas to match with 
#' new species dictionary information this needs to be updated to 52121. This file provides the necessary lookup
#' and can be used in conjunction with the BTOTools update_speccode() function. Based on  IOC version 13.2, 
#' last updated October 2023.
#'
#' @name speccode_mapping
#' @docType data
#' @format A data.frame with 403 rows and two columns
#' \describe{
#'   \item{numeric}{old_species_code = Numeric speceies code as used in older archived files}
#'   \item{numeric}{new_species_code = Numeric speceies code as used currently in Oracle etc}
#' }
#' @keywords data
NULL


#' 1-km squares in England
#' 
#' List of 1-km squares with land in England (see also lists for other countries. 
#' A square is listed for a country if it contains any land. Therefore, individual 
#' squares can be present in more than one list if they fall on a country border. 
#' Lists are based on low tide shapefiles so include some sand flats. Includes 
#' Isle of Man but not Channel Islands.
#' 
#' @name squares_01km_england
#' @docType data
#' @format A data.frame with 142562 rows and two columns
#' \describe{
#'   \item{character}{onekm = 1-km grid reference}
#'   \item{character}{country = country name}
#' }
#' @keywords data
NULL

#' 1-km squares in Scotland
#' 
#' List of 1-km squares with land in Scotland (see also lists for other countries. 
#' A square is listed for a country if it contains any land. Therefore, individual 
#' squares can be present in more than one list if they fall on a country border. 
#' Lists are based on low tide shapefiles so include some sand flats.
#' 
#' @name squares_01km_scotland
#' @docType data
#' @format A data.frame with 102797 rows and two columns
#' \describe{
#'   \item{character}{onekm = 1-km grid reference}
#'   \item{character}{country = country name}
#' }
#' @keywords data
NULL

#' 1-km squares in Wales
#' 
#' List of 1-km squares with land in Wales (see also lists for other countries. 
#' A square is listed for a country if it contains any land. Therefore, individual 
#' squares can be present in more than one list if they fall on a country border. 
#' Lists are based on low tide shapefiles so include some sand flats.
#' 
#' @name squares_01km_wales
#' @docType data
#' @format A data.frame with 24867 rows and two columns
#' \describe{
#'   \item{character}{onekm = 1-km grid reference}
#'   \item{character}{country = country name}
#' }
#' @keywords data
NULL

#' 1-km squares in Northern Ireland
#' 
#' List of 1-km squares with land in Northern Ireland (see also lists for other countries. 
#' A square is listed for a country if it contains any land. Therefore, individual 
#' squares can be present in more than one list if they fall on a country border. 
#' Lists are based on low tide shapefiles so include some sand flats.
#' 
#' @name squares_01km_northernireland
#' @docType data
#' @format A data.frame with 16102 rows and two columns
#' \describe{
#'   \item{character}{onekm = 1-km grid reference}
#'   \item{character}{country = country name}
#' }
#' @keywords data
NULL

#' 1-km squares in Republic of Ireland
#' 
#' List of 1-km squares with land in Republic of Ireland (see also lists for other countries. 
#' A square is listed for a country if it contains any land. Therefore, individual 
#' squares can be present in more than one list if they fall on a country border. 
#' Lists are based on low tide shapefiles so include some sand flats.
#' 
#' @name squares_01km_republicofireland
#' @docType data
#' @format A data.frame with 81513 rows and two columns
#' \describe{
#'   \item{character}{onekm = 1-km grid reference}
#'   \item{character}{country = country name}
#' }
#' @keywords data
NULL

#' 10-km squares assigned to dominant country
#' 
#' List of 10-km squares with land in Britain and Ireland where each square is 
#' assigned to a single country based on dominant land area in the square. 
#' Therefore, individual squares should be present in only one country.
#' Lists are based on low tide shapefiles so include some sand flats. Includes 
#' Isle of Man (assigned England) but not Channel Islands.
#' 
#' @name squares_10km_dominant_country
#' @docType data
#' @format A data.frame with 4154 rows and two columns
#' \describe{
#'   \item{character}{country = country name}
#'   \item{character}{tenkm = 10-km grid reference}
#' }
#' @keywords data
NULL


