#' Converts a column of grid references to columns of eastings and northings
#'
#' @description
#' Takes a dataframe of grid reference and produce new columns of eastings and 
#' northings. This function will work for British, Irish and Channel Islands grid references. 
#' But note that it does not translate between regions. i.e. Channel Islands grid references 
#' are converted to Channel Islands coordinates; Irish grid references are converted to Irish
#' coordinates. If you apply this function to a dataframe containing grid references from 
#' multiple regions it will return coordinates for multiple regions.
#'
#' @param df name of input dataframe
#' @param invar_gridref quoted string name of the variable containing the grid reference
#'
#' @return The same dataframe with additional easting and northing columns and region 
#' column indicating which grid system each row originates from, and therefore which grid
#' system the coordinates are from.
#'
#'
#' @examples
#' temp1 <- data.frame(other1 = 'A', other2 = 'B', 
#' square = c('TL1', 'TL12', 'TL12A', 'TL1234', 'TL123456', 'TL1234567', 'GB1234', 
#' 'TL12O', 'IN1234', 'WV2575'), 
#' stringsAsFactors = FALSE)
#' temp1a<-gridref_to_coordinates(temp1,'square')
#'
#' @export
#'
gridref_to_coordinates<-function(df, invar_gridref) {
  #check input parameters
  if(!is.data.frame(df)) stop('df must be a data frame')
  if(!is.character(invar_gridref)) stop('invar_gridref must be character strings')
  
  #check there aren't already easting and northing columns
  if('easting' %in% names(df)) stop('df already contains a column called easting')
  if('northing' %in% names(df)) stop('df already contains a column called northing')
  
  #copy to new df for processing so as not to overwrite any columns in original df
  temp_df <- df[c(invar_gridref)]
  
  #force name to be gridref to make easier processing
  names(temp_df)<- c('gridref')
  
  #make columns ready
  temp_df$easting <- NA
  temp_df$northing <- NA

  #add a sortorder so can return in original order
  temp_df$order <- as.numeric(row.names(temp_df))
  
  #check the resolution of the gridrefs - it may vary and could contain unprocessable strings
  temp_df$nchar <- nchar(temp_df$gridref)
  nchars <- unique(temp_df$nchar)
  nchars_acceptable <- c(4,5,6,8)
  
  #check for unprocessable rows
  temp_df$error <- 0
  for(c in 1:length(nchars)) {
    if(!nchars[c] %in% nchars_acceptable) warning(paste0('Dataframe contains grid references of unexpected length (',nchars[c],' characters). These rows will be skipped'))
    temp_df$error <- ifelse(!nchars[c] %in% nchars_acceptable & temp_df$nchar == nchars[c], 1, temp_df$error)
  }
  
  
  #split processable and unprocessable rows
  temp_df_errors <- subset(temp_df, error == 1)
  temp_df <- subset(temp_df, error == 0)
  
  #lookups needed below
  lookup_hundref <- data.frame(
    stringsAsFactors = FALSE,
    letters = c("SV","SW","SX",
                "SY","SZ","TV","SR","SS","ST","SU",
                "TQ","TR","SM","SN","SO","SP","TL",
                "TM","SH","SJ","SK","TF","TG","SC",
                "SD","SE","TA","NW","NX","NY","NZ",
                "NR","NS","NT","NU","NL","NM","NN",
                "NO","NF","NG","NH","NJ","NK",
                "NA","NB","NC","ND","HY","HZ","HT",
                "HU","HP",
                "WA", "WV",
                "IA", "IB", "IC", "ID",
                "IF", "IG", "IH", "IJ",
                "IL", "IM", "IN", "IO",
                "IQ", "IR", "IS", "IT",
                "IV", "IW", "IX", "IY"),
    e1000 = c(0L,1L,2L,3L,4L,5L,1L,
              2L,3L,4L,5L,6L,1L,2L,3L,4L,5L,6L,2L,3L,4L,
              5L,6L,2L,3L,4L,5L,1L,2L,3L,4L,1L,2L,3L,
              4L,0L,1L,2L,3L,0L,1L,2L,3L,4L,0L,1L,2L,3L,
              3L,4L,3L,4L,4L,
              5L, 5L,
              0L, 1L, 2L, 3L,
              0L, 1L, 2L, 3L,
              0L, 1L, 2L, 3L,
              0L, 1L, 2L, 3L,
              0L, 1L, 2L, 3L),
    n1000 = c(0L,0L,0L,0L,0L,0L,1L,
              1L,1L,1L,1L,1L,2L,2L,2L,2L,2L,2L,3L,3L,3L,
              3L,3L,4L,4L,4L,4L,5L,5L,5L,5L,6L,6L,6L,
              6L,7L,7L,7L,7L,8L,8L,8L,8L,8L,9L,9L,9L,9L,
              10L,10L,11L,11L,12L,
              55L,54L,
              4L, 4L, 4L, 4L,
              3L, 3L, 3L, 3L,
              2L, 2L, 2L, 2L,
              1L, 1L, 1L, 1L, 
              0L, 0L, 0L, 0L),
    region = c(rep('GB', 53), rep('CH', 2), rep('I', 20))
  )
  
  lookup_tetlet <- data.frame(
    stringsAsFactors = FALSE,
    tetlet = c("A","B","C","D","E",
               "F","G","H","I","J","K","L","M","N","P","Q",
               "R","S","T","U","V","W","X","Y","Z"),
    e10 = c(1L,1L,1L,1L,1L,3L,3L,
            3L,3L,3L,5L,5L,5L,5L,5L,7L,7L,7L,7L,7L,9L,
            9L,9L,9L,9L),
    n10 = c(1L,3L,5L,7L,9L,1L,3L,
            5L,7L,9L,1L,3L,5L,7L,9L,1L,3L,5L,7L,9L,1L,
            3L,5L,7L,9L)
  )
  
  
  
  #now process each grid ref resolution
  temp_df4 <- subset(temp_df, nchar == 4)
  if(nrow(temp_df4)>0) {
    temp_df4$letters <- substr(temp_df4$gridref,1,2)
    temp_df4 <- merge(temp_df4, lookup_hundref, by = 'letters', all.x = TRUE)
    temp_df4$e100 <- as.numeric(substr(temp_df4$gridref,3,3))
    temp_df4$n100 <- as.numeric(substr(temp_df4$gridref,4,4))
    temp_df4$e10 <- 5
    temp_df4$e1 <- 5
    temp_df4$n10 <- 5
    temp_df4$n1 <- 5
    temp_df4$easting <- (temp_df4$e1000*100000) + (temp_df4$e100*10000) + (temp_df4$e10*1000) + (temp_df4$e1*100) + 55
    temp_df4$northing <- (temp_df4$n1000*100000) + (temp_df4$n100*10000) + (temp_df4$n10*1000) + (temp_df4$n1*100) + 55
    temp_df4$letters <- NULL
    temp_df4$e1000 <- NULL
    temp_df4$e100 <- NULL
    temp_df4$e10 <- NULL
    temp_df4$e1 <- NULL
    temp_df4$n1000 <- NULL
    temp_df4$n100 <- NULL
    temp_df4$n10 <- NULL
    temp_df4$n1 <- NULL
  }
  temp_df5 <- subset(temp_df, nchar == 5)
  if(nrow(temp_df5)>0) {
    temp_df5$letters <- substr(temp_df5$gridref,1,2)
    temp_df5 <- merge(temp_df5, lookup_hundref, by = 'letters', all.x = TRUE)
    temp_df5$e100 <- as.numeric(substr(temp_df5$gridref,3,3))
    temp_df5$n100 <- as.numeric(substr(temp_df5$gridref,4,4))
    temp_df5$tetlet <- substr(temp_df5$gridref,5,5)
    temp_df5 <- merge(temp_df5, lookup_tetlet, by = 'tetlet', all.x = TRUE)
    temp_df5$e1 <- 0
    temp_df5$n1 <- 0
    temp_df5$easting <- (temp_df5$e1000*100000) + (temp_df5$e100*10000) + (temp_df5$e10*1000) + (temp_df5$e1*100)
    temp_df5$northing <- (temp_df5$n1000*100000) + (temp_df5$n100*10000) + (temp_df5$n10*1000) + (temp_df5$n1*100)
    temp_df5$letters <- NULL
    temp_df5$tetlet <- NULL
    temp_df5$e1000 <- NULL
    temp_df5$e100 <- NULL
    temp_df5$e10 <- NULL
    temp_df5$e1 <- NULL
    temp_df5$n1000 <- NULL
    temp_df5$n100 <- NULL
    temp_df5$n10 <- NULL
    temp_df5$n1 <- NULL
  }
  
  temp_df6 <- subset(temp_df, nchar == 6)
  if(nrow(temp_df6)>0) {
    temp_df6$letters <- substr(temp_df6$gridref,1,2)
    temp_df6 <- merge(temp_df6, lookup_hundref, by = 'letters', all.x = TRUE)
    temp_df6$e100 <- as.numeric(substr(temp_df6$gridref,3,3))
    temp_df6$e10 <- as.numeric(substr(temp_df6$gridref,4,4))
    temp_df6$n100 <- as.numeric(substr(temp_df6$gridref,5,5))
    temp_df6$n10 <- as.numeric(substr(temp_df6$gridref,6,6))
    temp_df6$e1 <- 5
    temp_df6$n1 <- 5
    temp_df6$easting <- (temp_df6$e1000*100000) + (temp_df6$e100*10000) + (temp_df6$e10*1000) + (temp_df6$e1*100) + 55
    temp_df6$northing <- (temp_df6$n1000*100000) + (temp_df6$n100*10000) + (temp_df6$n10*1000) + (temp_df6$n1*100) + 55
    temp_df6$letters <- NULL
    temp_df6$e1000 <- NULL
    temp_df6$e100 <- NULL
    temp_df6$e10 <- NULL
    temp_df6$e1 <- NULL
    temp_df6$n1000 <- NULL
    temp_df6$n100 <- NULL
    temp_df6$n10 <- NULL
    temp_df6$n1 <- NULL
    
  }
  
  temp_df8 <- subset(temp_df, nchar == 8)
  if(nrow(temp_df8)>0) {
    temp_df8$letters <- substr(temp_df8$gridref,1,2)
    temp_df8 <- merge(temp_df8, lookup_hundref, by = 'letters', all.x = TRUE)
    temp_df8$e100 <- as.numeric(substr(temp_df8$gridref,3,3))
    temp_df8$e10 <- as.numeric(substr(temp_df8$gridref,4,4))
    temp_df8$e1 <- as.numeric(substr(temp_df8$gridref,5,5))
    temp_df8$n100 <- as.numeric(substr(temp_df8$gridref,6,6))
    temp_df8$n10 <- as.numeric(substr(temp_df8$gridref,7,7))
    temp_df8$n1 <- as.numeric(substr(temp_df8$gridref,8,8))
    temp_df8$easting <- (temp_df8$e1000*100000) + (temp_df8$e100*10000) + (temp_df8$e10*1000) + (temp_df8$e1*100) + 55
    temp_df8$northing <- (temp_df8$n1000*100000) + (temp_df8$n100*10000) + (temp_df8$n10*1000) + (temp_df8$n1*100) + 55
    temp_df8$letters <- NULL
    temp_df8$e1000 <- NULL
    temp_df8$e100 <- NULL
    temp_df8$e10 <- NULL
    temp_df8$e1 <- NULL
    temp_df8$n1000 <- NULL
    temp_df8$n100 <- NULL
    temp_df8$n10 <- NULL
    temp_df8$n1 <- NULL
  }
  
  #join them all back together
  if(nrow(temp_df_errors)>0) temp_df_errors$region <- NA #add region column to errors df otherwise won't bind
  temp_done <- rbind(temp_df_errors, temp_df4, temp_df5, temp_df6, temp_df8)
  temp_done <- temp_done[order(temp_done$order),]
  temp_done$order <- NULL
  temp_done$error <- NULL
  temp_done$nchar <- NULL
  
  #check if any NAs returned and send warning
  if(anyNA(temp_done$easting) | anyNA(temp_done$northing)) 
    warning('NAs were returned; indicates at least one record could not be converted and may indicate error in grid reference')

  #check if more than one grid system being used
  regions <- unique(temp_done$region[!is.na(temp_done$region)])
  if(length(regions)>1)
    warning(paste0('More than one grid system represented in these data: ', paste(unlist(regions), collapse = ',')))
  
  #check df and temp_df still same length
  if(nrow(df) != nrow(temp_done)) stop('processed list not same length as original df')
  
  #add to original df and return
  df <- cbind(df, easting=temp_done$easting, northing=temp_done$northing)
  return(df)
}
