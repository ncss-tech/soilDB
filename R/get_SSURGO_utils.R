# function to build WSS urls
#' .make_WSS_download_url
#'
#' @param WHERE WHERE clause for query against `sacatalog`
#' @param include_template include template database?
#'
#' @return a vector of URLs to Web Soil Survey ZIP files
#' @noRd
.make_WSS_download_url <- function(WHERE = NULL, include_template = FALSE, db = c('SSURGO', 'STATSGO')) {
  
  db <- match.arg(toupper(db), c('SSURGO', 'STATSGO'))
  
  if (db == "STATSGO") {
    q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'US'"
    sacatalog <- SDA_query(q, dsn = NULL)
    res <- paste0(
      "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/STATSGO2/wss_gsmsoil_",
      WHERE, "_[", as.Date(sacatalog$saverest, format = "%m/%d/%Y %H:%M:%S"), "].zip"
    )
    unique(res)
  } else {
    # use SDA to get areasymbol and last updated date to build WSS cache urls
    q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol != 'US'"
    q2 <- ifelse(!is.null(WHERE), paste0(q, " AND (", WHERE, ")"), q)
    sacatalog <- SDA_query(q2, dsn = NULL)
    
    if (inherits(sacatalog, 'try-error') || is.null(sacatalog)) {
      return(try(stop("Query of Soil Survey Area Catalog (", 
                      WHERE, ") failed to return any data", call. = FALSE), silent = TRUE))
    }
    areasymbol <- sacatalog$areasymbol
    saverest <- sacatalog$saverest
    
    statecode <- substr(areasymbol, 0, 2)
    
    # handle custom (state-specific) template DBs
    # TODO: NPS urls cannot be derived from areasymbol alone
    # NB: no (current) usage of "NPS" template
    # TODO: authoritative source of release dates of templates?
    statecode[!statecode %in% c('AK','CT','FL','GA','HI',
                                'ID','IN','IA','ME','MI',
                                'MN','MT','NE','NJ','NC',
                                'OH','OR','PA','SD','UT',
                                'VT','WA','WV','WI','WY',
                                'HI','NPS')] <- "US"
    res <- paste0(
      "https://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_",
      areasymbol, ifelse(rep(include_template, length(areasymbol)), 
                         paste0("_soildb_", statecode, "_2003"), ""), "_[",
      as.Date(saverest, format = "%m/%d/%Y %H:%M:%S"), "].zip"
    )
    
    unique(res)
  }
}


#' .dumpSSURGOGDB
#'
#' Helper function for getting spatial(vector)/tabular data out of ESRI File Geodatabase (.gdb)
#'
#' @param dsn ESRI File Geodatabase path (ending with `".gdb"`)
#' @param exdir Parent directory to create `"./spatial/"` and `"./tabular/"` folders. May be a directory that does not yet exist.
#' @param replace_names Optional. Named character containing replacement names of form: `c("OLD"="NEW")`
#' @param header Default `TRUE` to include column names read from GDB. `FALSE` will assign column names based on metadata.
#' @return Dumps all tables to `exdir` as .txt (tabular) and .shp (vector) files
#' @keywords internal
#' @noRd
.dumpSSURGOGDB <- function(dsn, exdir, replace_names = NULL, header = TRUE) {
  if (!requireNamespace('sf')) {
    stop("package `sf` is required to extract tabular and spatial data from a File Geodatabase")
  }
  
  if (!dir.exists(file.path(exdir, "tabular")))
    dir.create(file.path(exdir, "tabular"), recursive = TRUE)
  
  if (!dir.exists(file.path(exdir, "spatial")))
    dir.create(file.path(exdir, "spatial"), recursive = TRUE)
  
  x <- sf::st_layers(dsn)
  lapply(seq_len(length(x$name)), function(i) {
    xn <- x$name[i]
    xg <- x$geomtype[i]
    d <- sf::read_sf(dsn, xn)
    if (!is.null(replace_names) && xn %in% names(replace_names)) {
      xn <- replace_names[xn]
    }
    if (is.na(xg)) {
      write.table(
        d,
        file = file.path(exdir, "tabular", paste0(xn, ".txt")),
        sep = "|",
        qmethod = "double",
        col.names = header,
        row.names = FALSE
      )
      d
    } else {
      sf::write_sf(d, file.path(exdir, "spatial", paste0(xn, ".shp")))
    }
  })
}


#' .prepare_RSS_raster
#'
#' Helper function for trimming raster exported from ESRI File Geodatabase.
#' OpenFileGDB driver is not able to export grid data, so other tools will
#' need to be used to create input TIFF file.
#'
#' @param x Input TIFF file
#' @param destfile Output TIFF File (default appends "_trim" to input filename)
#'
#' @return a trimmed SpatRaster with consistent NODATA (specified as IEEE 754 `NaN`)
#' @keywords internal
#' @noRd
.prepare_RSS_raster <- function(x, destfile = gsub('\\.tif$', '_trim.tif', x)) {
  
  if (!requireNamespace('terra'))
    stop("package `terra` is required to prepare Raster Soil Survey grids", call. = FALSE)
  
  r <- terra::rast(x)
  tf1 <- tempfile(fileext = ".tif")
  tf2 <- tempfile(fileext = ".tif")
  tf3 <- tempfile(fileext = ".tif")
  
  terra::NAflag(r) <- 0
  r2 <- terra::trim(r, filename = tf1, overwrite = TRUE)
  
  r3 <- terra::classify(r2, matrix(
    c(-2147483648, 1, 2147483647,
      2147483647, 2147483648, 2147483647),
    ncol = 3, byrow = TRUE
  ), include.lowest = TRUE, filename = tf2, overwrite = TRUE)
  
  terra::NAflag(r3) <- 2147483647
  r4 <- terra::trim(r3, filename = tf3, overwrite = TRUE)
  
  res <- terra::writeRaster(r4, filename = destfile, datatype = "INT4U", overwrite = TRUE)
  unlink(c(tf1, tf2, tf3))
  res
}

#' Apply an encoding function to a database
#'
#' @param dsn data source name (path to SQLite file)
#' @param FUN Default: `soilDB::code()`
#' @aliases .decode_db
#' @return Overwrites all tables in the database after applying encoding or decoding function
#' @noRd
#' @keywords internal
.recode_db <- function(dsn, FUN = soilDB::code) {
  if (requireNamespace("RSQLite")) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
    tables <- RSQLite::dbListTables(con)
    for (tt in tables) {
      res <- dbGetQuery(con, paste("SELECT * FROM", tt))
      res <- FUN(res)
      RSQLite::dbWriteTable(con, tt, res, overwrite = TRUE)
      message(tt)
    }
  }
}

#'@noRd
.decode_db <- function(dsn) {
  .recode_db(dsn, FUN = soilDB::uncode)
}

# internal function to get the full set of ids from NASIS to alias to export
.get_SSURGO_export_iid_table <- function(coiids) {
  dbQueryNASIS(NASIS(), sprintf("
      SELECT liid, lmapunitiid, muiid, corriid, dmuiid, coiid,
             areasymbol, musym, muname, compname, comppct_r
      FROM area
        INNER JOIN legend ON legend.areaiidref = area.areaiid
        INNER JOIN lmapunit ON lmapunit.liidref = legend.liid
        INNER JOIN mapunit ON mapunit.muiid = lmapunit.muiidref
        INNER JOIN correlation ON correlation.muiidref = mapunit.muiid
        INNER JOIN datamapunit ON datamapunit.dmuiid = correlation.dmuiidref
        INNER JOIN component ON component.dmuiidref = datamapunit.dmuiid
      WHERE component.coiid IN %s", format_SQL_in_statement(coiids)))
}


#' Fetch data from a custom NASIS SSURGO Export (MS Access)
#'
#' To create a custom NASIS SSURGO Export, go to Exports Explorer menu and select "Add New Export..."
#'
#' Tab #1: Criteria
#' - Select Export Target: `SSURGO`
#' - Choose desired map units, data map units, and components
#'
#' Tabs #2 and #3: Interpretations and Notes
#' - Select Interpretations to include in export
#' - Select Text Notes to include in export
#'
#' Tab #4: Run Export
#'  - Enter file name for ZIP
#'  - Export run on server and result sent to e-mail associated with user
#'
#' @param dsn Path to SQLite database containing SSURGO schema.
#' @noRd
#' @keywords internal
#' @return data.frame containing interpretation rules and ratings
.get_SSURGO_interp <- function(dsn, rules = NULL) {
  
  # interpretations are unique to component
  idcols <- c("lmapunitiid","coiid")
  
  # TODO: include low, high, low RV in addition to default "high RV"
  rulecols <- c("mrulename", "rating", "class", "reasons")
  
  # using same logic as the get_SDA* methods for multi-rule results
  .cleanRuleColumnName <- function(x) gsub("[^A-Za-z0-9]", "", x)
  ruleresult <-  lapply(rules,
                        function(rule) {
                          subres <- .get_SSURGO_export_interp_reasons_by_mrulename(dsn, rule)[, c(idcols, rulecols)]
                          newcolnames <- colnames(subres)
                          newcolnames[newcolnames %in% rulecols] <- paste0(newcolnames[newcolnames %in% rulecols],
                                                                           "_", .cleanRuleColumnName(rule))
                          colnames(subres) <- newcolnames
                          subres
                        })
  
  # left join each flattened rule result sequentially
  last <- ruleresult[[1]]
  for (i in 2:length(ruleresult)) {
    last <- merge(last, ruleresult[[i]], by = c("lmapunitiid", "coiid"))
  }
  last
}


#' Get Interpretation Rating "Reasons" from SSURGO Export
#'
#' @param dsn Path to SQ
#' @param mrulename Rule name of interpretation
#' @param n Number of reasons to return
#' @noRd
#' @keywords internal
#' @return A `data.frame` containing columns: "lmapunitiid", "coiid", "mrulename", "cokeyref", "Reasons", "liid", "muiid", "corriid", "dmuiid", "areasymbol", "musym", "compname", "comppct_r", "interphr", "interphrc","mukey"
# @examples
#
# library(data.table)
#
# # dsn: path to with a local SSURGO export (containing cointerp table)
# dsn <- "db.gpkg"
#
# # mrulename: select a rule name (must exist in export referenced by dsn)
# mrulename <-  "WMS - Pond Reservoir Area" #"FOR - Mechanical Site Preparation (Surface)"
#
# result <- data.table(.get_SSURGO_export_interp_reasons_by_mrulename(dsn, mrulename))
#
# result_ssurgo <- get_SDA_interpretation(mrulename,
#                                         method = "NONE",
#                                         areasymbols = c("CA630","CA649"))
#
# x1 <- data.table(mukey = result$mukey,
#                  compname = result$compname,
#                  rating_new = result$interphr,
#                  rating_class = result$interphrc,
#                  rating_new_reason = result$Reason)
#
# x2 <- data.table(mukey = result_ssurgo$MUKEY,
#                  compname = result_ssurgo$compname,
#                  rating_old = result_ssurgo$rating,
#                  rating_class = result_ssurgo$class)
#
# combined_result <- x2[x1, on = c("mukey","compname")]
#
# combined_result$CHECK <- round(combined_result$rating_new, 2) == round(combined_result$rating_old, 2)
.get_SSURGO_export_interp_reasons_by_mrulename <- function(dsn, mrulename, n = 2) {
  # based on VBA Function in Report Functions module of above .mdb
  # GetInterpReasons(strCokey As String,
  #                  strMRuleName As String,
  #                  intReasonCount As Integer) As Variant
  .N <- NULL
  .SD <- NULL
  cokey <- NULL
  if (grepl("\\.mdb", dsn)) {
    channel <- DBI::dbConnect(odbc::odbc(),
                              .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  } else if (grepl("\\.sqlite", dsn)) {
    channel <- DBI::dbConnect(RSQLite::SQLite(), dsn)
  } else {
    stop("invalid dsn", call. = FALSE)
  }
  
  q <- sprintf("SELECT * FROM cointerp
                WHERE [mrulename] = '%s' AND [ruledepth] = 0
                ORDER BY [interphr] DESC", mrulename)
  cointerpbase <- data.table::as.data.table(DBI::dbGetQuery(channel, q))
  
  # TODO: allow extend reasons to rules with ruledepth > 1?
  q <- sprintf("SELECT cokey, interphrc FROM cointerp
                WHERE [mrulename] = '%s' AND [ruledepth] = 1
                ORDER BY [interphr] DESC", mrulename)
  res <- data.table::as.data.table(DBI::dbGetQuery(channel, q))
  
  # identify the key components of the cointerp table to relate to NASIS
  cointerpkey <- data.frame(do.call('rbind', strsplit(cointerpbase$cointerpkey, ":")))
  colnames(cointerpkey) <- c("lmapunitiid","coiid","mrulekey","seqnum")
  
  # unique "cokey" is a composition of mukey (SSURGO) plus coiid (NASIS)
  cointerpbase$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", cointerpbase$cokey))
  res$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", res$cokey))
  cointerpbase$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", cointerpbase$cokey))
  res$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", res$cokey))
  
  # get lookup table
  res2 <- .get_SSURGO_export_iid_table(cointerpkey$coiid)
  
  # extract the "high representative" rating and class for 0th level rule
  high_rep_rating_class <- cointerpbase[,c("lmapunitiid","coiid","interphr","interphrc")]
  colnames(high_rep_rating_class) <- c("lmapunitiid","coiid","interphr","interphrc")
  
  # flatten the reasons so they are 1:1 with component, join to lookup tables
  result <- as.data.frame(res[, list(mrulename = unique(mrulename),
                                     cokeyref = unique(cokey),
                                     reasons = paste(.SD[["interphrc"]][1:pmin(.N, n)], collapse = "; ")),
                              by = c("lmapunitiid", "coiid")][res2,
                                                              on = c("lmapunitiid", "coiid")][high_rep_rating_class,
                                                                                              on = c("lmapunitiid","coiid")])
  
  result$rating <- result$interphr
  result$class <- result$interphrc
  # add mukey:lmapunitiid alias for convenience
  result$mukey <- result$lmapunitiid
  
  return(result)
}

#' .mdb2sqlite
#'
#' Convert a MS Access .mdb file to a SQLite .sqlite file by transferring all tables.
#'
#' @param dsn Path to MS Access .mdb file.
#' @param dsn_out Path to output SQLite (.sqlite) file. Default replaces `".mdb"` with `".sqlite"`.
#' @param quiet logical. Suppress error messages? Default: `FALSE`
#'
#' @return a list containing elements either `data.frame` result of table read from .mdb file, or `try-error` on error
#' @noRd
#' @keywords internal
#' @examples
#' \dontrun{
#' mdb2sqlite("template.mdb")
#' }
.mdb2sqlite <- function(dsn, dsn_out = gsub("\\.mdb", ".sqlite", dsn), quiet = FALSE) {
  channel <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  channel_out <- DBI::dbConnect(RSQLite::SQLite(), dsn_out)
  tables <- DBI::dbListTables(channel)
  res <- lapply(tables, function(x) {
    y <- try(DBI::dbReadTable(channel, x), silent = quiet)
    if (!inherits(y, 'try-error')) DBI::dbWriteTable(channel_out, x, y)
  })
  DBI::dbDisconnect(channel)
  DBI::dbDisconnect(channel_out)
  res
}
