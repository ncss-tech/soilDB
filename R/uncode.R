#' Convert coded values returned from NASIS and SDA queries to factors
#'
#' These functions convert the coded values returned from NASIS or SDA to
#' factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For SDA
#' the metadata is pulled from a static snapshot in the soilDB package
#' (/data/metadata.rda).
#'
#' These functions convert the coded values returned from NASIS into their
#' plain text representation. It duplicates the functionality of the CODELABEL
#' function found in NASIS. This function is primarily intended to be used
#' internally by other soilDB R functions, in order to minimizes the need to
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
#' Beware the default is to return the values as factors rather than strings.
#' While strings are generally preferable, factors make plotting more
#' convenient. Generally the factor level ordering returned by uncode() follows
#' the naturally ordering of categories that would be expected (e.g. sand,
#' silt, clay).
#'
#' @aliases metadata uncode code
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
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors?
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A data frame with the results.
#' @author Stephen Roecker
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet() &
#'     require(aqp)) {
#'   # query component by nationalmusym
#'   comp <- fetchSDA(WHERE = "nationalmusym = '2vzcp'")
#'   s <- site(comp)
#'
#'   # use SDA uncoding domain via db argument
#'   s <- uncode(s,  db="SDA")
#'   levels(s$taxorder)
#' }
#' }
#'
#' @export uncode
uncode <- function(df,
                   invert = FALSE,
                   db = "NASIS",
                   droplevels = FALSE,
                   stringsAsFactors = default.stringsAsFactors(),
                   dsn = NULL) {

  get_metadata <- function() {

    q <- "SELECT mdd.DomainID, DomainName, ChoiceSequence, ChoiceValue, ChoiceName,
                 ChoiceLabel, ColumnPhysicalName, ColumnLogicalName, ChoiceObsolete, ChoiceDescription
          FROM MetadataDomainDetail mdd
            INNER JOIN MetadataDomainMaster mdm ON mdm.DomainID = mdd.DomainID
            INNER JOIN (SELECT MIN(DomainID) DomainID, MIN(ColumnPhysicalName) ColumnPhysicalName, MIN(ColumnLogicalName) ColumnLogicalName
                        FROM MetadataTableColumn GROUP BY DomainID, ColumnPhysicalName) mtc ON mtc.DomainID = mdd.DomainID
          ORDER BY mdd.DomainID, ColumnPhysicalName, ChoiceValue;"

    channel <- dbConnectNASIS(dsn)

    if (inherits(channel, 'try-error'))
      return(data.frame())

    # exec query
    d <- dbQueryNASIS(channel, q)

    # done
    return(d)
  }

  # load current metadata table
  if (db == "NASIS") {
    metadata <- get_metadata()
  } else {
      load(system.file("data/metadata.rda", package = "soilDB")[1])
  }

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
  for (i in columnsToWorkOn.idx){

    # get the current metadata
    sub <- metadata[metadata[[metadata_col]] %in% nm[i], ]

    # NASIS or LIMS
    if (db %in% c("NASIS", "LIMS")) {
      if (invert == FALSE){
        # replace codes with values
        df[, i] <- factor(df[, i], levels = sub[[value_col]], labels = sub[[name_col]])
      } else {
        # replace values with codes
        df[, i] <- factor(df[, i], levels = sub[[name_col]], labels = sub[[value_col]])}
    }

    # SDA
    if (db == "SDA") {
      if (invert == FALSE){
        # replace codes with values
        df[, i] <- factor(df[, i], levels = sub[[label_col]])
      } else {
        # replace values with codes
        df[, i] <- factor(df[, i], levels = sub[[label_col]], labels = sub[[value_col]])
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


