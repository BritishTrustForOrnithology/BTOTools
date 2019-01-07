#' Species dictionary lookup
#'
#' Common names, scientific names, two-letter codes, sort order and taxonomic rank for birds, mammals and various inverts
#'
#' @name global_species_lookup
#' @docType data
#' @format A data.frame with 34051 rows and eight columns
#' \describe{
#'   \item{numeric}{Numeric taxon key code}
#'   \item{character}{Species' scientific name}
#'   \item{character}{Species' common name}
#'   \item{numeric}{Taxonomic rank identification (numeric) of entity}
#'   \item{character}{Taxonomic rank of entity}
#'   \item{numeric}{Sort order under IOC}
#'   \item{character}{Two-letter code if exists}
#' }
#' @references IOC
#' @keywords data
NULL

#' Species' scientific name synonyms
#'
#' A list of previously used scientific names and their currently used synonym, to assist in updating taxonomy 
#' of old data files
#'
#' @name sciname_synonyms
#' @docType data
#' @format A data.frame with 3087 rows and 4 columns
#' \describe{
#'   \item{numeric}{Numeric taxon key code}
#'   \item{character}{Formerly used scientific name}
#'   \item{character}{Currently used scientific name}
#'   \item{numeric}{Species sort order}
#' }
#' @keywords data
NULL