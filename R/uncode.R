uncode <- function(df, 
                   invert = FALSE, 
                   db = "NASIS", 
                   droplevels = FALSE, 
                   stringsAsFactors = default.stringsAsFactors()
                   ) {
  get_metadata <- function() {
    # must have RODBC installed
    if(!requireNamespace('RODBC'))
      stop('please install the `RODBC` package', call.=FALSE)
    
    q <- "SELECT mdd.DomainID, DomainName, ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel, ChoiceDescription, ColumnPhysicalName, ColumnLogicalName, ChoiceObsolete
    
    FROM MetadataDomainDetail mdd
    INNER JOIN MetadataDomainMaster mdm ON mdm.DomainID = mdd.DomainID
    INNER JOIN (SELECT MIN(DomainID) DomainID, MIN(ColumnPhysicalName) ColumnPhysicalName, MIN(ColumnLogicalName) ColumnLogicalName FROM MetadataTableColumn GROUP BY DomainID, ColumnPhysicalName) mtc ON mtc.DomainID = mdd.DomainID
    
    ORDER BY DomainID, ColumnPhysicalName, ChoiceValue;"
    
    # setup connection local NASIS
    channel <- RODBC::odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials'))
    
    # exec query
    d <- RODBC::sqlQuery(channel, q, stringsAsFactors = FALSE)
    
    # close connection
    RODBC::odbcClose(channel)
    
    # done
    return(d)
  }
  
  # load current metadata table
  if (db == "NASIS"){
    metadata <- get_metadata()
    } else {
      load(system.file("data/metadata.rda", package="soilDB")[1])
      }
  
  # unique set of possible columns that will need replacement
  possibleReplacements <- unique(metadata$ColumnPhysicalName)
  
  # names of raw data
  nm <- names(df)
  # index to columns with codes to be replaced
  columnsToWorkOn.idx <- which(nm %in% possibleReplacements)
  
  # iterate over columns with codes
  for (i in columnsToWorkOn.idx){
    
    # get the current metadata
    sub <- metadata[metadata$ColumnPhysicalName %in% nm[i], ]
    
    # NASIS or LIMS
    if (db %in% c("NASIS", "LIMS")) {
      if (invert == FALSE){
        # replace codes with values
        df[, i] <- factor(df[, i], levels = sub$ChoiceValue, labels = sub$ChoiceName)
      } else { 
        # replace values with codes
        df[, i] <- factor(df[, i], levels = sub$ChoiceName, labels = sub$ChoiceValue)}
    }
    
    # SDA
    if (db == "SDA") {
      if (invert == FALSE){
        # replace codes with values
        df[, i] <- factor(df[, i], levels = sub$ChoiceLabel)
      } else {
        # replace values with codes
        df[, i] <- factor(df[, i], levels = sub$ChoiceLabel, labels = sub$ChoiceValue)
        }
      }
    }
  
  # drop unused levels
  if (droplevels == TRUE) {
    idx <- which(! nm %in% possibleReplacements)
    df <- droplevels(df, except = idx)
    }
  
  # convert factors to strings
  if (stringsAsFactors == FALSE) {
    idx <- unlist(lapply(df, is.factor))
    df[idx] <- lapply(df[idx], as.character)
  }
  
  return(df)
}

# convenient, inverted version of uncode()
code <- function(df, ...) {
  res <- uncode(df, invert=TRUE, ...)
  return(res)
}


