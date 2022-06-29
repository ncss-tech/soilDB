#' Convert coded values returned from NASIS and SDA queries into human-readable values
#'
#' These functions convert the coded values returned from NASIS or SDA to
#' factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For SDA
#' the metadata is pulled from a static snapshot in the soilDB package
#' (/data/metadata.rda).
#' 
#' @details 
#' These functions convert the coded values returned from NASIS into their
#' plain text representation. It duplicates the functionality of the CODELABEL
#' function found in NASIS. This function is primarily intended to be used
#' internally by other soilDB R functions, in order to minimize the need to
#' manually convert values.
#'
#' The function works by iterating through the column names in a data frame and
#' looking up whether they match any of the ColumnPhysicalNames found in the
#' metadata domain tables. If matches are found then the columns coded values
#' are converted to their corresponding factor levels. Therefore it is not
#' advisable to reuse column names from NASIS unless the contents match the
#' range of values and format found in NASIS. Otherwise uncode() will convert
#' their values to NA.
#'
#' When data is being imported from NASIS, the metadata tables are sourced
#' directly from NASIS. When data is being imported from SDA or the NASIS Web
#' Reports, the metadata is pulled from a static snapshot in the soilDB
#' package.
#' 
#' Set `options(soilDB.NASIS.skip_uncode = TRUE)` to bypass decoding logic; for instance when using soilDB NASIS functions with custom NASIS snapshots that have already been decoded.
#'
#' @aliases uncode code
#' 
#' @param df data.frame
#'
#' @param invert converts the code labels back to their coded values (`FALSE`)
#'
#' @param db label specifying the soil database the data is coming from, which
#' indicates whether or not to query metadata from local NASIS database
#' ("NASIS") or use soilDB-local snapshot ("LIMS" or "SDA")
#'
#' @param droplevels logical: indicating whether to drop unused levels in
#' classifying factors. This is useful when a class has large number of unused
#' classes, which can waste space in tables and figures.
#'
#' @param stringsAsFactors deprecated
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A `data.frame` with the results.
#' @author Stephen Roecker
#' @keywords manip
#' @export
#' @examples
#' # convert column name `fraghard` (fragment hardness) codes to labels
#' uncode(data.frame(fraghard = 1:10))
#' 
#' # convert column name `fragshp` (fragment shape) labels to codes
#' code(data.frame(fragshp = c("flat", "nonflat")))
uncode <- function(df,
                   invert = FALSE,
                   db = "NASIS",
                   droplevels = FALSE,
                   stringsAsFactors = NULL,
                   dsn = NULL) {
  
  if (!missing(db)) {
    .Deprecated(msg = "passing `db` argument to uncode is no longer necessary, lookups are based on ChoiceName and/or ChoiceLabel")
  }
  
  if (getOption("soilDB.NASIS.skip_uncode", default = FALSE)) {
    # some static instances of NASIS come pre-decoded
    return(df)
  }
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  metadata <- get_NASIS_metadata(dsn = dsn)

  # unique set of possible columns that will need replacement
  metadata_col <- names(metadata)[grep("ColumnPhysicalName", names(metadata), ignore.case = TRUE)]
  name_col <- names(metadata)[grep("ChoiceName", names(metadata), ignore.case = TRUE)]
  value_col <- names(metadata)[grep("ChoiceValue", names(metadata), ignore.case = TRUE)]
  label_col <- names(metadata)[grep("ChoiceLabel", names(metadata), ignore.case = TRUE)]
  possibleReplacements <- unique(metadata[[metadata_col]])

  # names of raw data
  nm <- names(df)
  # index to columns with codes to be replaced
  columnsToWorkOn.idx <- which(nm %in% possibleReplacements)

  # iterate over columns with codes
  for (i in columnsToWorkOn.idx) {
    # get the current metadata
    sub <- metadata[metadata[[metadata_col]] %in% nm[i],]
    
    if (!invert) {
      # replace values with ChoiceName, try filling NA with replace based on ChoiceLabel
      # do not explicitly set `levels` if none of the values in value_col (numeric) are present
      if (any(df[[i]] %in% sub[[value_col]])) {
        nc <- factor(df[[i]], levels = sub[[value_col]], labels = sub[[name_col]])
        lc <- factor(df[[i]], levels = sub[[value_col]], labels = sub[[label_col]])
        if (all(is.na(nc))) {
          df[[i]] <- lc
        } else {
          nc[is.na(nc)] <- lc[is.na(nc)]
          df[[i]] <- nc
        }
        df[[i]] <- nc
      } else {
        nc <- factor(df[[i]], levels = sub[[name_col]], labels = sub[[name_col]])
        lc <- factor(df[[i]], levels = sub[[label_col]], labels = sub[[label_col]])
        if (all(is.na(nc))) {
          df[[i]] <- lc
        } else {
          nc[is.na(nc)] <- lc[is.na(nc)]
          df[[i]] <- nc
        }
      }
    } else if (invert) {
      # replace values with ChoiceName, try filling NA with replace based on ChoiceLabel
      nc <- factor(df[[i]], levels = sub[[name_col]], labels = sub[[value_col]])
      lc <- factor(df[[i]], levels = sub[[label_col]], labels = sub[[value_col]])
      if (all(is.na(nc))) {
        df[[i]] <- lc
      } else {
        nc[is.na(nc)] <- lc[is.na(nc)]
        df[[i]] <- nc
      }
    }
  }

  # drop unused levels
  if (droplevels) {
    idx <- which(!nm %in% possibleReplacements)
    df <- droplevels(df, except = idx)
  }

  # convert factors to strings, check soilDB option first
  if ((length(stringsAsFactors) > 0 && !stringsAsFactors) ||
      !getOption("soilDB.NASIS.DomainsAsFactor", default = FALSE)) {
    idx <- unlist(lapply(df, is.factor))
    df[idx] <- lapply(df[idx], as.character)
  }
  
  return(df)
}

# convenient, inverted version of uncode()
#' @export
#' @rdname uncode
code <- function(df, 
                 db = "NASIS",
                 droplevels = FALSE,
                 stringsAsFactors = NULL,
                 dsn = NULL) {
  res <- uncode(df, invert = TRUE, db = db, droplevels = droplevels, stringsAsFactors = stringsAsFactors, dsn = dsn) 
  return(res)
}

#' Get/Set Options for Encoding NASIS Domains as Factors
#' 
#' Set package option `soilDB.NASIS.DomainsAsFactor` for returning coded NASIS domains as factors. 
#'
#' @param x logical; default `FALSE`
#'
#' @return logical, result of `getOption("soilDB.NASIS.DomainsAsFactor")`
#' @export
#'
#' @examples
#' \dontrun{
#' NASISDomansAsFactor(TRUE)
#' }
NASISDomainsAsFactor <- function(x = NULL) {
  if (!is.null(x)) {
     options(soilDB.NASIS.DomainsAsFactor = (x || getOption("stringsAsFactors", default = FALSE)))
  }
  invisible(getOption("soilDB.NASIS.DomainsAsFactor", default = FALSE))
}

#' Get NASIS Metadata (Domain, Column and Choice Lists)
#' 
#' Retrieve a table containing domain and column names with choice list labels/names/sequences/values from the NASIS 7 metadata tables.
#' 
#' These data are derived from the MetadataDomainDetail, MetadataDomainMaster, and MetadataTableColumn tables and help with mapping between values stored in the NASIS database and human-readable values. The human-readable values align with the values returned in public facing interfaces such as SSURGO via Soil Data Access and NASIS Web Reports. The data in these tables can also be used to create _ordered_ factors where options for levels of a particular data element follow a logical `ChoiceSequence`.
#'
#' @param dsn Optional: path to local SQLite database containing NASIS table structure; default: `NULL`
#' 
#' @details If a local NASIS instance is set up, and this is the first time `get_NASIS_metadata()` has been called, the metadata will be obtained from the NASIS local database. Subsequent runs in the same session will use a copy of the data object `NASIS.metadata` cached in `soilDB.env`.
#' 
#' For users without a local NASIS instance, a cached copy of the NASIS metadata are used `(data/metadata.rda)`. 
#' 
#' See `?soilDB::metadata` for additional details.
#' 
#' @return a `data.frame` containing DomainID, DomainName, DomainRanked, DisplayLabel, ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel, ChoiceObsolete, ColumnPhysicalName, ColumnLogicalName
#' @export
#' 
#' @examples
#' get_NASIS_metadata()
get_NASIS_metadata <- function(dsn = NULL) {
  
  metadata <- NULL
  
  .doQuery <- function(dsn){
    q <- "SELECT mdd.DomainID, DomainName, DomainRanked, DisplayLabel, 
                 ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel, ChoiceObsolete, 
                 ColumnPhysicalName, ColumnLogicalName
            FROM MetadataDomainDetail mdd
              INNER JOIN MetadataDomainMaster mdm ON mdm.DomainID = mdd.DomainID
              INNER JOIN (SELECT MIN(DomainID) DomainID, MIN(ColumnPhysicalName) ColumnPhysicalName, MIN(ColumnLogicalName) ColumnLogicalName
                          FROM MetadataTableColumn GROUP BY DomainID, ColumnPhysicalName) mtc ON mtc.DomainID = mdd.DomainID
            ORDER BY mdd.DomainID, ColumnPhysicalName, ChoiceValue;"
    
    channel <- dbConnectNASIS(dsn)
    
    if (inherits(channel, 'try-error'))
      return(data.frame())
    
    # exec query
    dbQueryNASIS(channel, q)
  }
  
  # load current metadata table
  if (local_NASIS_defined(dsn = dsn)) {
    
    # cache NASIS metadata in soilDB.env within an R session
    if (!exists("NASIS.metadata", envir = soilDB.env)) {
      metadata <- .doQuery(dsn = dsn)
      assign('NASIS.metadata', value = metadata, envir = soilDB.env)
    } else {
      metadata <- get("NASIS.metadata", envir = soilDB.env)
    }
    
  } 
  
  if (is.null(metadata) || (is.data.frame(metadata) && nrow(metadata) == 0)) {
    load(system.file("data/metadata.rda", package = "soilDB")[1])
  }

  metadata
}

#' Get NASIS metadata entries for specific domains or choices
#'
#' @param x character vector to match in NASIS metadata
#' @param what Column to match `x` against. Default `"ColumnPhysicalName"`; alternate options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`, `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`, `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`, `"ColumnLogicalName"`
#' @return a `data.frame` containing selected NASIS metadata sorted first on `DomainID` and then on `ChoiceSequence`
#' @export
#' @rdname get_NASIS_metadata
#' @examples
#' get_NASIS_column_metadata("texcl")
get_NASIS_column_metadata <- function(x, 
                                      what = "ColumnPhysicalName",
                                      dsn = NULL) {
  metadata <- get_NASIS_metadata(dsn = dsn)
  mds <- metadata[metadata[[what]] %in% x, ] 
  mds <- mds[order(mds$DomainID, mds$ChoiceSequence), ]
  mds
}

#' @keywords internal
#' @noRd
.get_NASIS_metadata <- function(dsn = NULL) {
  # for backward compatibility or anyone who is using the .get method in the wild
  .Deprecated("get_NASIS_metadata")
  get_NASIS_metadata(dsn)
}
