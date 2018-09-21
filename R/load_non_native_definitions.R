#' Load lists of species considered to be established vs exotic non-natives
#'
#' @description
#' For some analyses it may be appropriate to distinguish between native and non-native species,
#' and to potentially separate the latter into those with established populations (Category C)
#' and those yet to establish (Category E or exotic). Some species can be safely considered
#' to be non-natives in the breedng season (e.g. Red-breasted Goose) whereas in winter true
#' wild vagrants can occur. Hence a different classification is provided for winter and breeding.
#'
#' @return A dataframe with the following columns:
#' \item{speccode}{numeric species code}
#' \item{non_native_b}{species definition for breeding data = C=established, E=exotic}
#' \item{non_native_w}{species definition for winter data = C=established, E=exotic}
#'
#' @examples
#' df<-load_non_native_definitions()
#'
#' @export
#'
load_non_native_definitions<-function() {
  df<-read.csv(paste0(.BTOarchive_path,'birdatlas2007-11/data/lookups/non_native_categories.csv')
                 ,header=T
                 ,colClasses=c('numeric','character','character')
  )
  return(df)
}
