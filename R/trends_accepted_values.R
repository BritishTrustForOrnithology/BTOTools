# named list of accepted values for the birdtrends app. Used in the validation checker func
# library(tibble)

#' Accepted values for Birdtrends Explorer datasets
#' 
#' A list of accepted values for datasets used in Birdtrends Explorer. This includes values for certain columns, as well as column, value, requirement, and type constraints for metadata, graph data, and change data files. The items in the list are as follows:
#' 
#' @format A list with 7 items:
#' \describe{
#'  \item{types}{`metric_type` values - high-level descriptor of the quality measured}
#'  \item{names}{`metric_name` values - accessible descriptors of the trends for a general audience}
#'  \item{areas}{`area` values - geographic area for a trend}
#'  \item{schemes}{`source_scheme` values - source scheme names}
#'  \item{metadata}{tibble containing attribute names (written in the `attribute` column), their types, and whether they require a corresponding value in the `value` column for the metadata file}
#'  \item{graph_data}{tibble containing column names, their types, and whether they're mandatory to include for the graph data file}
#'  \item{change_data}{tibble containing column names, their types, and whether they're mandatory to include for the change data file}
#' }
#' 
#' @import tibble
#' @export
#' @md
#' 
trends_vals <- list(
  # metric types and names
  types = c(
    "Population abundance",
    "Distribution",
    "Breeding performance",
    "Survival and mortality",
    "Timing of life cycle events"
  ),
  
  names = c(
    "Long-term trend (1994–)",
    "Long-term trend (1998–) along linear waterways",
    "Long-term trend (1970s–) along linear waterways",
    "Long-term trend (1960s–)",
    "Long-term trend",
    # "Long-term trend of non-breeding waterbirds",
    "Long-term trend at heronries",
    "Long-term trend at colonies",
    "Long-term winter trend at wetlands",
    "Annual winter census counts",
    "Range size",
    "Clutch size",
    "Brood size",
    "Egg-stage nest failure rate",
    "Chick-stage nest failure rate",
    "Fledglings per breeding attempt",
    "Adult survival (recoveries)",
    "Adult survival (RAS)",
    "Timing of breeding",
    "Onset of primary moult",
    "Whole-season productivity (CES)",
    "Adult abundance (CES)",
    "Adult survival (CES)"
  ),
  
  areas = c(
    'United Kingdom',
    'Britain & Ireland',
    'Great Britain',
    'England',
    'England+Wales',
    'Scotland',
    'Wales',
    'Northern Ireland',
    'Channel Islands',
    'Isle of Man',
    'All Ireland',
    'Anglian and North East Regions combined'
  ),
  
  # source scheme names
  schemes = c(
    "BBS",
    "Birdatlas range size",
    "CBC-BBS",
    "CES abundance",
    "CES productivity",
    "CES survival",
    "GSMP",
    "Heronries Census",
    "NRS brood size",
    "NRS chick-stage nest failure rate",
    "NRS clutch size",
    "NRS egg-stage nest failure rate",
    "NRS laying date",
    "NRS productivity",
    "RAS survival",
    "RBBP",
    "Ringing moult timing",
    "Ringing survival",
    "SMP colony counts",
    "SMP productivity",
    "WeBS",
    "WBBS",
    "WBS-WBBS"
  ),
  
  metadata = tibble(
    name = c(
      "source_scheme",
      "report_year",
      "metric_type",
      "metric_name",
      "graph_caption",
      "licence",
      "attribution",
      "citation",
      "subgroup",
      "indexed",
      "index_base_year",
      "index_base_value",
      "graph_units",
      "graph_hline",
      "unsmoothed_ci_type",
      "smooth_method",
      "smoothed_ci_type",
      "sample_size_definition",
      "change_units",
      "change_ci_type",
      "citation_url"
    ),
    class = c(
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character"
    ),
    required = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      #       this is for licence - should it be made true and then a warning pops up pointing out what it defaults to?
      FALSE, 
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE)
  ),
  
  
  
  graph_data = tibble(
    name = c(
      "source_scheme",
      "metric_name",
      "master_taxon_id",
      "english_name",
      "area",
      "report_year",
      "year",
      "unsmoothed_estimate",
      "subgroup",
      "unsmoothed_se",
      "unsmoothed_lower_ci",
      "unsmoothed_upper_ci",
      "smoothed_estimate",
      "smoothed_lower_ci",
      "smoothed_upper_ci",
      "sample_size",
      "year_warning",
      "publish"
    ),
    class = c(
      "character",
      "character",
      "integer",
      "character",
      "character",
      "integer",
      "integer",
      "numeric",
      "character",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "integer",
      "character",
      "logical"
    ),
    
    required = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  ),
  
  
  
  change_data = tibble(
    name = c(
      "source_scheme",
      "metric_name",
      "master_taxon_id",
      "english_name",
      "area",
      "report_year",
      "year_start",
      "year_end",
      "period_length",
      "change_estimate",
      "subgroup",
      "change_lower_ci",
      "change_upper_ci",
      "change_significance",
      "change_sample_size",
      "warning",
      "alert",
      "publish"
    ),
    class = c(
      "character",
      "character",
      "integer",
      "character",
      "character",
      "integer",
      "integer",
      "integer",
      "integer",
      "numeric",
      "character",
      "numeric",
      "numeric",
      "character",
      "integer",
      "character",
      "character",
      "logical"
    ),
    required = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )
  
  
  
)


# forcibly assign attributes meant to be integers as integer class since readr automatically goes to numeric
# only if original column was numeric, however so can catch completely wrong class types such as character
assign_integer_class <- function(df){
  # all columns that need to be converted to int
  cols <- c("master_taxon_id","report_year","year","sample_size",
            "change_sample_size","period_length","year_start","year_end")
  
  df <- df %>%
    mutate(
      # any_of doesn't check for missing variables so won't throw an error
      across(
        c(where(is.numeric) &
            any_of(cols)
        ),
        as.integer
      )
    )
  
  return(df)
  
}
