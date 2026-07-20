# library(dplyr)

# comment out when in library
# source("R/trends_accepted_values.R")


#' Check Birdtrends dataset(s) for errors
#'
#' Check a single dataset or folder of datasets to see if it conforms to the Birdtrends dataset format, and output specific errors if it doesn't  
#' 
#' This is a strict check, and may flag up issues that don't actually exist within the dataset, for example, optional columns that are allowed to be empty will be flagged when checking whether the dataset contains blank columns. Another example is that columns with some missing values (for example, estimate values that weren't able to be calculated for a specific year, and hence are NA) will also be flagged. 
#' 
#' The function checks for various things, including: 
#' * Do files have the correct number of mandatory columns and (for metadata) values in the attribute column?
#' * Are names - column names, file names, values in the attribute column (for metadata) - following conventions and using accepted values only? 
#' * Do certain columns - source scheme name, metric type, metric names, area names - use accepted values only?
#' * Are columns the correct type?
#' * Are there empty rows?
#' * Are values in the source scheme name and report year columns the same as the values stated within in the filename?
#' For specific details, check the formatting document. The accepted scheme values and column types can be viewed in [trends_vals].
#' 
#' Additionally, beyond certain attribute (for metadata), column name (for graph and change data), and file name checks, the function is unable to check for errors in the actual values inputted, such as typos. The function also does not currently check whether species names follow the current IOC English names list as adopted in [global_species_lookup].
#' 
#' Note: a warning may be output when reading certain files that looks like this:
#' ```
#' One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#' dat <- vroom(...)
#' problems(dat)
#' ```
#' This is a warning that the readr::read_csv() function outputs when it encounters uneven column counts for each row, most often produced after manipulating metadata files within Excel. It will often invisibly merge these 'trailing' columns together, and so far has not produced any issue when compiling data. Therefore, it can be ignored.
#' 
#' @examplesIf interactive()
#' check_birdtrends_dataset("data/BBS")
#' check_birdtrends_dataset("data/BBS/metadata_2000 BBS.csv")
#' 
#' @param path Path to single .csv dataset, or folder containing .csv datasets
#' @return None
#' 
#' @import dplyr
#' @export
#' @md
#' 
check_birdtrends_dataset <- function(path) {
  is_dir <- file_test("-d", path)
  if (is_dir) {
    rlang::inform("Directory detected. Reading .csv files within.")
    files <- list.files(path = path, pattern = '*.csv', full.names = TRUE)
    
    if (length(files) == 0) {
      rlang::abort("No .csv files found within folder")
    }
    
    # assign to variable otherwise will output return values to stdout
    x <- lapply(files, function(file) {
      
      log <- list()
      rlang::inform(paste("\nChecking", file))
      rlang::try_fetch(
        {
          errors <- check_single_dataset(file, log)
          log <- append(log, errors)
          
        },
        # subclass to target abort errors -
        # all abort errors need this subclass in order to be caught by this catch func
        execution_halt = function(cnd) {
          # rethrow abort condition as a warning instead so execution can continue with next file
          # whilst allowing the abort function when checking a single file to actually stop execution
          
          rlang::warn(
            c(
              paste0("* Execution-halting error for ", file, ": ", cnd$message)
              
            ),
            parent = cnd, class = "abort_rethrown" )
          
          return(NA)
        },
        abort_rethrown = function(cnd) {
          # adds abort error to log
          # specific class rather than warn bc otherwise will catch e.g. parse warnings from readr
          log <<- append(log, cnd$message)
          
          return(0)
        }
        
      )
      
      
      output_errors(log)
    })
  }
  else {
    log <- list()
    log <- check_single_dataset(path, log)
    
    output_errors(log)
  }
}

output_errors <- function(log) {
  if (length(log) > 0) {
    rlang::inform(
      c(
        "Issues found when checking:"
      )
      
    )
    
    for (i in 1:length(log)) {
      rlang::inform(
        paste0(log[[i]])
        
      )
    }
    
    
    
  } else {
    rlang::inform(
      "No issues detected ✓"
    )
  }
}

check_single_dataset <- function(filepath, log) {
  
  # file existence check
  if (!file.exists(filepath)) {
    rlang::abort(paste("File", filepath, "does not exist"),
                 class = "execution_halt")
  }
  
  filename <- basename(filepath)
  # to check against later on
  info <- check_filename(filename)
  
  # check file encoding - 
  # this isn't extremely important to fix since changing encoding can be difficult, but good to point out
  guess_enc <- readr::guess_encoding(filepath)
  rlang::inform(
    c(
      paste("File uses encoding standard", guess_enc[[1]][1]),
      "i" = "If possible, files should use UTF-8 encoding."
    )
  )
  enc <- ifelse(guess_enc[[1]][1] == "windows-1252", "WINDOWS-1252", "UTF-8")
  
  # this often happens - can ignore
  # One or more parsing issues, call `problems()` on your data frame for details, e.g.:
  # dat <- vroom(...)
  # problems(dat) 
  df <- readr::read_csv(filepath, na = c('', 'NA'), locale = readr::locale(encoding = enc), col_types = readr::cols())
  
  
  df <- assign_integer_class(df)
  
  empty_cols <- c()
  for (name in colnames(df)) {
    col <- df[[name]]
    if (all(is.na(col) | col == "")) {
      # some cols are allowed to be empty... how to differentiate between genuinely empty cols and this? set col name for missing cols manually?
      # keep empty cols since some may be genuine file attributes (albeit optional)
      empty_cols <- c(empty_cols, name)
      # rlang::inform(paste(name, "detected as empty column."))
    }    
  }
  
  if (length(empty_cols) > 0) {
    message <- paste0("* ", length(empty_cols), " empty columns detected in data: ", paste0(empty_cols, collapse = ", "),". These columns may be either optional attribute columns or mistakenly included blank columns.")
    log <- append(log, message)
    
  }
  
  na_rows <- apply(df, 1, function(row) {all(is.na(row))})
  empty_rows <- sum(na_rows)
  
  if (empty_rows > 0) {
    indices <- which( na_rows == TRUE)
    message <- paste("*", empty_rows, "empty row(s) detected in data at index(es):", paste(indices, collapse = ", "))
    log <- append(log, message)
    
    df <- df[!na_rows,]
  } 
  
  rlang::inform(paste("Data type detected as", info$type))
  
  
  
  # do file type specific checks now
  if (info$type == "metadata") {
    # check number of columns is correct
    if (length(names(df))!= 2) {
      # NOTE: when this issue is encountered whilst running through a whole folder abort error doesn't show up for some reason? seems to be subsumed by the vroom parsing error that can occur if wrong delim value is used
      rlang::abort(
        paste0("Incorrect number of columns for metadata file. Expected 2 columns with column headers 'attribute, value', found ", length(names(df)), " columns with column headers '", paste0(names(df), collapse = ","),"'."),
        class = "execution_halt")
    }
    
    # check that col names are correct
    if (names(df)[[1]] == "attribute" & names(df)[[2]] == "value") {
      # remove trailing and leading whitespace
      df$attribute <- stringr::str_trim(df$attribute)
      log <- check_metadata(df, info, log)
      
    } else {
      rlang::abort(paste0("Incorrect column headers for metadata file. Expected 'attribute, value', found '", paste0(names(df),  collapse = ", "),"'"),
                   class = "execution_halt")
    }
  } else if (info$type == "graph_data") {
    requirements <- trends_vals$graph_data
    log <- check_data(df, info, requirements, log)
  } else if (info$type == "change_data") {
    requirements <- trends_vals$change_data
    log <- check_data(df, info, requirements, log)
    
  }
  
  return(log)
  
}


check_metadata <- function(df, info, log) {
  
  requirements <- trends_vals$metadata
  
  cols <- df %>% 
    rename(name = attribute)
  #for checking class types later
  cols$class_observed <- unlist(lapply(cols$value,class))
  
  # merge together - full join so can check missing accepted fields and fields that aren't accepted
  joined <- dplyr::full_join(requirements, cols[c("name", "value", "class_observed")], by = "name")
  
  # check for issues with each row
  # priority: missing, invalid, value needed (mandatory), wrong type (detected vs what it should be)
  errors <- joined %>% 
    mutate(
      issue = case_when(
        !name %in% cols$name ~ "missing attribute",
        !name %in% requirements$name ~ "invalid attribute name",
        required == TRUE & is.na(value) ~ "missing value (mandatory attribute)",
        class_observed != class ~ paste("incorrect class type detected (expected", class, "got", class_observed, ")")
        
      )
    )
  
  # filter out rows without issues and clean columns
  errors <- errors %>% 
    filter(!is.na(issue)) %>% 
    select(name, issue) %>% 
    rename(attribute = name)
  
  
  if (nrow(errors) > 0) {
    message <- c(
      paste(nrow(errors), "issues found with metadata attributes:"),
      knitr::kable(errors)
    )
    
    log <- append(log, message)
    
    # if ("citation_url" %in% errors$attribute) {
    #   log <- append(log, paste("Note: citation_url is "))
    # }
  }
  
  # individual column/logical checks - checking if metric type is from list of accepted values etc.
  
  if ("source_scheme" %in% cols$name & cols$value[cols$name == "source_scheme"] != info$scheme) {
    message <- paste0("* ","source_scheme attribute should be the same as in the file name. Expected: '", info$scheme, "', got '", cols$value[cols$name == "source_scheme"], "'")
    
    log <- append(log, message)
  }
  
  if ("report_year" %in% cols$name & cols$value[cols$name == "report_year"] != info$year) {
    message <- paste0("*","report_year attribute should be the same as in the file name. Expected: '", info$year, "', got '", cols$value[cols$name == "report_year"], "'")
    
    log <- append(log, message)
    
  }
  
  if ("metric_type" %in% cols$name & !cols$value[cols$name == "metric_type"] %in% trends_vals$types ) {
    message <- c(
      paste("*",cols$value[cols$name == "metric_type"], "is not an accepted metric_type attribute"),
      "i" = paste("Accepted values:", paste0(trends_vals$types, collapse = ", " ))
      
    )
    
    log <- append(log, message)
  }
  
  if ("metric_name" %in% cols$name & !cols$value[cols$name == "metric_name"] %in% trends_vals$names ) {
    message <- c(
      "*",paste(cols$value[cols$name == "metric_name"], "is not an accepted metric_name attribute"),
      "i" = paste0("Accepted values: ", paste0(trends_vals$names, collapse = ", " ),". Note that year ranges use endashes (–), not hyphens (-).")
    )
    
    log <- append(log, message)
  }
  
  return(log)
}


# graph and change data
check_data <- function(df, info, requirements, log) {
  
  df_list <- as.list(df)
  
  # column information - name, is column completely empty, does col have empty fields, and class typing if column isn't empty
  cols <- tibble(
    name = names(df_list),
    empty = sapply(df_list, function(col) {all(is.na(col))}),
    has_missing = sapply(df_list, function(col) {any(is.na(col))})
  )
  
  cols$class_observed <- unlist(lapply(df_list,class))
  
  # for columns observed to be empty, remove the class observed to not mess up type checks
  cols <- cols %>% mutate(
    class_observed = if_else(empty, NA, class_observed)
  )
  
  # merge together - full join so can check missing accepted fields and fields that aren't accepted
  joined <- dplyr::full_join(requirements, cols, by = "name")
  
  # check for issues with each row
  # required but missing, invalid attribute name, required but empty, (missing values found in required col - add?), class wrong
  errors <- joined %>% 
    mutate(
      issue = case_when(
        required == TRUE & !name %in% cols$name ~ "missing mandatory attribute",
        !name %in% requirements$name ~ "invalid attribute name",
        # TODO: another case for missing values in mandatory attribute? 
        required == TRUE & empty == TRUE ~ "mandatory attribute has empty column",
        required == TRUE & has_missing == TRUE ~ "mandatory attribute has missing values",
        empty == FALSE & class_observed != class ~ paste0("incorrect class type detected (expected ", class, ", got ", class_observed, ")")
        
      )
    )
  
  # filter out rows without issues and clean columns
  errors <- errors %>% 
    filter(!is.na(issue)) %>% 
    select(name, issue) %>% 
    rename(column = name)
  
  
  if (nrow(errors) > 0) {
    message <- c(
      paste("*",nrow(errors), "potential issues found with data columns:"),
      knitr::kable(errors)
    )
    
    log <- append(log, message)
    
    
  }
  
  values <- cols %>% 
    filter(empty == FALSE & (name == "source_scheme" | name == "report_year" | name ==  "metric_type" | name =="metric_name" | name == "area"))
  
  values$value_observed <- apply(values, 1, function(row){
    unique(df[[row["name"]]])
  })
  
  # individual column/logical checks - checking if metric type is from list of accepted values etc.
  # also does a length check for some cols, since columns like source scheme and metric type should only contain one value
  
  if ("source_scheme" %in% values$name) {
    row_val <- values$value_observed[values$name == "source_scheme"]
    if (length(row_val) > 1) {
      
      message <- paste("*","Multiple source_scheme values detected:", row_val )
      log <- append(log, message)
      
    } else if (row_val != info$scheme) {
      
      message <- paste0("* ","source_scheme attribute should be the same as in the file name. Expected: '", info$scheme, "', got '", row_val, "'")
      log <- append(log, message)
    }
  }
  
  if ("report_year" %in% values$name) {
    row_val <- values$value_observed[values$name == "report_year"]
    if (length(row_val) > 1) {
      message <- paste("*","Multiple report_year values detected:", row_val )
      log <- append(log, message)
      
    } else if (row_val != info$year) {
      message <- paste0("* ","report_year attribute should be the same as in the file name. Expected: '", info$year, "', got '", row_val, "'")
      log <- append(log, message)
    }
  }
  
  # checks to see if values for these columns are in "accepted values"
  if ("metric_type" %in% values$name) {
    row_val <- values$value_observed[values$name == "metric_type"]
    if (length(row_val) > 1) {
      message <- paste("*","Multiple metric_type values detected:", row_val )
      log <- append(log, message)
    } else if (!row_val %in% trends_vals$types) {
      message <- c(
        paste("*",row_val, "is not an accepted metric_type attribute"),
        "i" = paste("Accepted values:", paste0(trends_vals$types, collapse = ", " ))
      )
      
      log <- append(log, message)
      
    }
  }
  
  if ("metric_name" %in% values$name) {
    row_val <- values$value_observed[values$name == "metric_name"]
    if (length(row_val) > 1) {
      message <- paste("*","Multiple metric_name values detected:", row_val )
      log <- append(log, message)
    } else if (!row_val %in% trends_vals$names) {
      message <- c(
        paste("*",row_val, "is not an accepted metric_name attribute"),
        "i" = paste("Accepted values:", paste0(trends_vals$names, collapse = ", " ))
      )
      
      log <- append(log, message)
    }
  }
  
  if ("area" %in% values$name) {
    row_val <- values$value_observed[values$name == "area"][[1]]
    if (!all(row_val %in% trends_vals$areas)) {
      
      wrong_areas <- row_val[!row_val %in% trends_vals$areas]#
      message <- c(
        paste("*",wrong_areas, "is/are not (an) accepted area value(s)"),
        "i" = paste("Accepted values:", paste0(trends_vals$areas, collapse = ", " ))
      )
      
      log <- append(log, message)
    }
  }
  
  # TODO: check names are all proper names from the IOC?
  
  return(log)
  
}


check_filename <- function(filename) {
  if (!grepl("\\.csv$", filename)) {
    rlang::abort(paste("File", filename, "should be in .csv format"),
                 class = "execution_halt")
  }
  
  type <- ""
  if (startsWith(filename, "metadata_")) {
    type <- "metadata"
  } else if (startsWith(filename, "graph_data_")) {
    type <- "graph_data"
  } else if (startsWith(filename, "change_data_")) {
    type <- "change_data"
  } else {
    rlang::abort(paste("Data type unrecognised. The filename must start with one of: metadata, change_data, or graph_data followed by an underscore."),
                 class = "execution_halt")
  }
  
  
  # remove data type and .csv extension
  filename <- stringr::str_remove(filename, paste0(type,"_"))
  filename <- stringr::str_remove(filename, "\\.csv")
  # extract year and scheme name - fixed since scheme names can also have spaces
  rtn <- stringr::str_split_fixed(filename, " ", 2)
  
  # if can't split into two sections, stop
  if (length(rtn) != 2) {
    rlang::abort("Year and scheme name can't be extracted from the filename.",
                 class = "execution_halt")
  }
  year <- rtn[[1]]
  scheme <- rtn[[2]]
  
  # check if year is numeric 4 digits long
  # show quotation marks around year to reveal blank spaces if any 
  if (!stringr::str_detect(year, "\\d{4}")) {
    rlang::abort(paste0("Year value should consist of 4 digits. Detected: '", year, "'"),
                 class = "execution_halt")
  }
  
  # check if scheme name exists 
  # show quotation marks to reveal trailing/leading blank spaces if any 
  if (!scheme %in% trends_vals$schemes) {
    rlang::abort(
      c(
        paste0("Scheme '", scheme, "' is not an accepted scheme name value. Scheme names must be separated by blank spaces, not underscores." ),
        "i" = paste0("Accepted values: ", paste0(trends_vals$schemes, collapse = ", "))
      ),
      class = "execution_halt"
    )
  }
  
  return(list(type = type, scheme = scheme, year = year))
  
  
}



