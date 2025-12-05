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
                   
                   dsn = NULL) {
  if (!missing(db)) {
    .Deprecated(msg = "passing `db` argument to uncode is no longer necessary, lookups are based on ChoiceName and/or ChoiceLabel")
  }
  
  if (getOption("soilDB.NASIS.skip_uncode", default = FALSE)) {
    # some static instances of NASIS come pre-decoded
    return(df)
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
      } else {
        nc <- factor(df[[i]], levels = sub[[name_col]], labels = sub[[name_col]])
        lc <- factor(df[[i]], levels = sub[[label_col]], labels = sub[[label_col]])
      }
      if (sum(is.na(nc)) > sum(is.na(lc))) {
        df[[i]] <- lc
      } else {
        df[[i]] <- nc
      }
    } else if (invert) {
      # replace values with ChoiceName, try filling NA with replace based on ChoiceLabel
      nc <- factor(df[[i]], levels = sub[[name_col]], labels = sub[[value_col]])
      lc <- factor(df[[i]], levels = sub[[label_col]], labels = sub[[value_col]])
      if (sum(is.na(nc)) > sum(is.na(lc))) {
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

  # convert factors to strings/numeric, check soilDB option first
  if (invert || !NASISDomainsAsFactor()) {
    idx <- unlist(lapply(df, is.factor))
    df[idx] <- lapply(df[idx], function(x) {
      type.convert(x, as.is = TRUE)
    })
  }
  
  return(df)
}

# convenient, inverted version of uncode()
#' @export
#' @rdname uncode
code <- function(df, 
                 db = NULL,
                 droplevels = FALSE,
                 dsn = NULL) {
  res <- uncode(df, invert = TRUE, droplevels = droplevels, dsn = dsn) 
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
#' @param dsn Optional: path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: `NULL`
#' @param include_description Include "ChoiceDescription" column? Default: `FALSE`
#' @details If a local NASIS instance is set up, and this is the first time `get_NASIS_metadata()` has been called, the metadata will be obtained from the NASIS local database. Subsequent runs in the same session will use a copy of the data object `NASIS.metadata` cached in `soilDB.env` which can be accessed with `get_soilDB_env()$NASIS.metadata`.
#' 
#' For users without a local NASIS instance, a cached copy of the NASIS metadata are used `(data/metadata.rda)`. 
#' 
#' See `?soilDB::metadata` for additional details.
#' 
#' @return a `data.frame` containing DomainID, DomainName, DomainRanked, DisplayLabel, ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel, ChoiceObsolete, ColumnPhysicalName, ColumnLogicalName and optionally ChoiceDescription when `include_description=TRUE`.
#' @export
get_NASIS_metadata <- function(dsn = NULL, include_description = FALSE) {
  
  metadata <- NULL
  
  .doQuery <- function(dsn){
    q <- sprintf("SELECT mdd.DomainID, DomainName, DomainRanked, DisplayLabel, 
                 ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel, ChoiceObsolete, 
                 ColumnPhysicalName, ColumnLogicalName %s
            FROM MetadataDomainDetail mdd
              INNER JOIN MetadataDomainMaster mdm ON mdm.DomainID = mdd.DomainID
              INNER JOIN (SELECT MIN(DomainID) DomainID, MIN(ColumnPhysicalName) ColumnPhysicalName, MIN(ColumnLogicalName) ColumnLogicalName
                          FROM MetadataTableColumn GROUP BY DomainID, ColumnPhysicalName) mtc ON mtc.DomainID = mdd.DomainID
            ORDER BY mdd.DomainID, ColumnPhysicalName, ChoiceValue;", 
            ifelse(include_description, ", ChoiceDescription", ""))
    
    channel <- dbConnectNASIS(dsn)
    
    if (inherits(channel, 'try-error'))
      return(data.frame())
    
    # exec query
    dbQueryNASIS(channel, q)
  }
  
  # load current metadata table
  if (local_NASIS_defined(dsn = dsn)) {
    
    # cache NASIS metadata in soilDB.env within an R session
    if (!exists("NASIS.metadata", envir = get_soilDB_env())) {
      metadata <- .doQuery(dsn = dsn)
      assign('NASIS.metadata', value = metadata, envir = get_soilDB_env())
    } else {
      metadata <- get("NASIS.metadata", envir = get_soilDB_env())
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
                                      include_description = FALSE,
                                      dsn = NULL) {
  metadata <- get_NASIS_metadata(dsn = dsn, include_description = include_description)
  mds <- metadata[metadata[[what]] %in% x, ] 
  mds <- mds[order(mds$DomainID, mds$ChoiceSequence), ]
  mds
}

#' Work with NASIS Choice Lists
#'
#' Create (ordered) factors and interchange between choice names, values and labels for lists of input vectors.
#'
#' @param x A named list of vectors to use as input for NASIS Choice List lookup
#' @param colnames vector of values of the column specified by `what`. E.g. `colnames="texcl"` for `what="ColumnPhysicalName"`. Default: `names(x)` (if x is named)
#' @param what passed to `get_NASIS_column_metadata()`; Column to match `x` against. Default `"ColumnPhysicalName"`; alternate options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`, `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`, `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`, `"ColumnLogicalName"`
#' @param choice one of: `"ChoiceName"`, `"ChoiceValue"`, or `"ChoiceLabel"`
#' @param obsolete Include "obsolete" choices? Default: `FALSE`
#' @param factor Convert result to factor? Default: `TRUE`
#' @param droplevels Drop unused factor levels? Default: `TRUE` (used only when `factor=TRUE`)
#' @param ordered Should the result be an ordered factor? Default: `TRUE` (use _only_ if `DomainRanked` is true for all choices)
#' @param simplify Should list result with length 1 be reduced to a single vector? Default: `TRUE`
#' @param dsn Optional: path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: NULL
#' @return A list of "choices" based on the input `x` that have been converted to a consistent target set of levels (specified by `choice`) via NASIS 7 metadata. 
#' 
#' When `factor=TRUE` the result is a factor, possibly ordered when `ordered=TRUE` and the target domain is a "ranked" domain (i.e. `ChoiceSequence` has logical meaning).
#' 
#' When `factor=FALSE` the result is a character or numeric vector. Numeric vectors are always returned when `choice` is `"ChoiceValue"`.
#' 
#' @export
#'
#' @examples
#' 
#' NASISChoiceList(1:3, "texcl")
#' 
#' NASISChoiceList(1:3, "pondfreqcl")
#' 
#' NASISChoiceList("Clay loam", "texcl", choice = "ChoiceValue")
#' 
#' NASISChoiceList("Silty clay loam", "texcl", choice = "ChoiceName")
NASISChoiceList <- function(x = NULL,
           colnames = names(x),
           what = "ColumnPhysicalName",
           choice = c("ChoiceName", "ChoiceValue", "ChoiceLabel"),
           obsolete = FALSE,
           factor = TRUE,
           droplevels = FALSE,
           ordered = TRUE,
           simplify = TRUE,
           dsn = NULL) {
  choice <- match.arg(choice, choices = c("ChoiceName", "ChoiceValue", "ChoiceLabel"))
  if (!is.list(x)) {
    n <- colnames
    x <- list(x)
    if (length(n) == length(x)) {
      names(x) <- n
    }
  }
  res <- lapply(names(x), function(xx) {
    y <- get_NASIS_column_metadata(xx, what = what, dsn = dsn)
    if (!obsolete) {
      y <- y[y$ChoiceObsolete == 0, ]
    }
    
    # create lut of value:name:choice
    lut <- do.call('cbind', lapply(y[c("ChoiceValue", "ChoiceName", "ChoiceLabel")],
                                   function(xxx) match(x[[xx]], xxx)))
    
    # pick the best (possibly there is overlap between name and choice)
    lut.y <- lut[, which.max(apply(lut, MARGIN = 2, function(xxx) sum(!is.na(xxx))))]
    yy <- y[lut.y,]
    
    if (choice != "ChoiceValue" && factor) {
      newlevels <- y[[choice]]
      if (all(is.na(y$DomainRanked) | y$DomainRanked == 0)) {
        ordered <- FALSE
      } else {
        # if ordered and domain is ranked, reorder levels based on sequence if needed
        newlevels <- newlevels[order(y[["ChoiceSequence"]])]
      }
      f <- factor(yy[[choice]], 
                  levels = newlevels, 
                  ordered = ordered)
      if (droplevels) {
        return(droplevels(x))
      } else {
        return(f)
      }
    } else {
      return(yy[[choice]])
    }
  })
  
  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  res
}

#' @keywords internal
#' @noRd
.get_NASIS_metadata <- function(dsn = NULL) {
  # for backward compatibility or anyone who is using the .get method in the wild
  .Deprecated("get_NASIS_metadata")
  get_NASIS_metadata(dsn)
}

#' Get NASIS Table Metadata (Table and Column Descriptions)
#' 
#' Retrieve a table containing table and column names with descriptions, help text, units of measure, etc. from NASIS 7 metadata tables.
#' 
#' These data are derived from the MetadataTable and MetadataTableColumn tables and describe the expected contents of standard NASIS tables and columns.
#' @param table Character vector of table identifiers to match. Default `NULL` for "all tables" (no constraint)
#' @param column Character vector of column identifiers to match. Default `NULL` for "all columns" (in selected tables, if any, otherwise no constraint)
#' @param what.table Column to match `table` against. Default: `TablePhysicalName`.
#' @param what.column Column to match `column` against. Default: `ColumnPhysicalName`.
#' @param query_string Default: `FALSE`; if `TRUE` return a character containing query that would be sent to NASIS.
#' @param dsn Optional: path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: `NULL`
#' @details For NASIS choice lists based on domain and column names see `get_NASIS_metadata()` and `NASISChoiceList()`. This function (`get_NASIS_table_metadata()`) is intended for higher-level description of the expected contents of a NASIS database instance, rather than the codes/specific values used within columns.
#' @seealso `get_NASIS_metadata()` `NASISChoiceList()` `uncode()` `code()`
#' @return a `data.frame` 
#' @export
#' @examples
#' if (local_NASIS_defined())
#'  str(get_NASIS_table_metadata())
get_NASIS_table_metadata <- function(table = NULL, column = NULL, 
                                     what.table = "TablePhysicalName", 
                                     what.column = "ColumnPhysicalName",
                                     query_string = FALSE, dsn = NULL) {
  constraint <- ""
  
  if (!is.null(table) || !is.null(column)) {
    constraint <- " WHERE "
  }
  
  if (!is.null(table)) {
    constraint <- paste0(constraint, what.table, " IN ", format_SQL_in_statement(table))
  }
  
  if (!is.null(table) && !is.null(column)) {
    constraint <- paste0(constraint, " AND ")
  }
  
  if (!is.null(column)) {
    constraint <- paste0(constraint, what.column, " IN ", format_SQL_in_statement(column))
  }
  
  q <- paste0("SELECT MetadataTableColumn.TableID AS TableID, TablePhysicalName, TableLogicalName, TableLabel,
                      TableDescription, ImportExportFileName, TableCollectionID, DAGLevel,
                      MetadataTable.Visible AS MetadataTable_Visible,
                      Selectable, Editable, NoInsertOrDelete, RootTable, CreateAsView, ClientDatabaseOnly,
                      ServerDatabaseOnly, ColumnID, TableColumnSequence, BaseColumnPhysicalName,
                      ColumnPhysicalName, ColumnLogicalName, ColumnGroupLabel, ColumnLabel,
                      PhysicalDataType, LogicalDataType, DomainID, ColumnDescription, ColumnHelpText,
                      FieldSize, DecimalPrecision, DatetimePrecision, Minimum, Maximum, DefaultType,
                      DefaultValue, Alignment, DisplaySize, SortSequence, SortType, SortDirection,
                      UnitsOfMeasureUnabbreviated, UnitsOfMeasureAbbreviated, [NotNull],
                      MetadataTableColumn.Visible AS MetadataTableColumn_Visible,
                      Protected, SetDefaultOnObjectChange, SetDefaultOnRowChange,
                      IncludeInReplicationSelectList, FileContentColumnID
        FROM MetadataTable
        INNER JOIN MetadataTableColumn ON MetadataTable.TableID = MetadataTableColumn.TableID
        ", constraint)
  
  if (query_string) {
    return(q)
  }
  
  uncode(dbQueryNASIS(NASIS(dsn = dsn), q))
}
