#' Get SSURGO ZIP files from Web Soil Survey 'Download Soils Data'
#' 
#' Download ZIP files containing spatial (ESRI shapefile) and tabular (TXT) files with standard SSURGO format; optionally including the corresponding SSURGO Template Database with `include_template=TRUE`.
#' 
#' To specify the Soil Survey Areas you would like to obtain data you use a `WHERE` clause for query of `sacatalog` table such as `areasymbol = 'CA067'`, `"areasymbol IN ('CA628', 'CA067')"` or  `areasymbol LIKE 'CT%'`.
#'
#' @param WHERE A SQL `WHERE` clause expression used to filter records in `sacatalog` table. Alternately `WHERE` can be any spatial object supported by `SDA_spatialQuery()` for defining the target extent.
#' @param destdir Directory to download ZIP files into. Default `tempdir()`.
#' @param exdir Directory to extract ZIP archives into. May be a directory that does not yet exist. Each ZIP file will extract to a folder labeled with `areasymbol` in this directory. Default: `destdir`
#' @param include_template Include the (possibly state-specific) MS Access template database? Default: `FALSE`
#' @param extract Logical. Extract ZIP files to `exdir`? Default: `TRUE`
#' @param remove_zip Logical. Remove ZIP files after extracting? Default: `FALSE` 
#' @param overwrite Logical. Overwrite by re-extracting if directory already exists? Default: `FALSE`
#' @param quiet Logical. Passed to `download.file()`.
#' @export
#' 
#' @details Pipe-delimited TXT files are found in _/tabular/_ folder extracted from a SSURGO ZIP. The files are named for tables in the SSURGO schema. There is no header / the files do not have column names. See the _Soil Data Access Tables and Columns Report_: \url{https://sdmdataaccess.nrcs.usda.gov/documents/TablesAndColumnsReport.pdf} for details on tables, column names and metadata including the default sequence of columns used in TXT files.
#' 
#' Several ESRI shapefiles are found in the _/spatial/_ folder extracted from a SSURGO ZIP. These have prefix `soilmu_` (mapunit), `soilsa_` (survey area), `soilsf_` (special features). There will also be a TXT file with prefix `soilsf_` describing any special features. Shapefile names then have an `a_` (polygon), `l_` (line), `p_` (point) followed by the soil survey area symbol.
#' 
#' @return Character. Paths to downloaded ZIP files (invisibly). May not exist if `remove_zip = TRUE`.

downloadSSURGO <- function(WHERE, 
                           destdir = tempdir(), 
                           exdir = destdir, 
                           include_template = FALSE,
                           extract = TRUE, 
                           remove_zip = FALSE,
                           overwrite = FALSE,
                           quiet = FALSE) {
  
  if (!is.character(WHERE)) {
    # attempt passing to SDA_spatialQuery
    res <- suppressMessages(SDA_spatialQuery(WHERE, what = 'areasymbol'))
    WHERE <- paste("areasymbol IN", format_SQL_in_statement(res$areasymbol))
  }
  
  # make WSS download URLs from areasymbol, template, date
  urls <- .make_WSS_download_url(WHERE, include_template = include_template)
  
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }
  
  # download files
  for (i in seq_along(urls)) {
    destfile <- file.path(destdir, basename(urls[i]))
    if (!file.exists(destfile)) {
      try(download.file(urls[i], destfile = destfile, quiet = quiet, mode = "wb"))
    }
  }
  
  paths <- list.files(destdir, pattern = "\\.zip$", full.names = TRUE)
  paths2 <- paths[grep(".*wss_SSA_(.*)_.*", paths)]
  
  if  (extract) {
    if (!quiet) {
      message("Extracting downloaded ZIP files...")
    }
    
    if (length(paths2) == 0) {
      stop("Could not find SSURGO ZIP files in `destdir`: ", destdir, call. = FALSE)
    }
    
    if (!dir.exists(exdir)) {
      dir.create(exdir, recursive = TRUE)
    }
    
    for (i in seq_along(paths2)) {
      ssa <- gsub(".*wss_SSA_(.*)_soildb.*", "\\1", paths2[i])
      if ((!dir.exists(file.path(exdir, ssa)) || overwrite) && 
          length(utils::unzip(paths2[i], exdir = exdir)) == 0) {
        message(paste('Invalid zipfile:', paths2[i]))
      }
    }
    
    if (remove_zip) {
      file.remove(paths2)
    }
  }
  
  invisible(paths2)
}

# function to build WSS urls
.make_WSS_download_url <- function(WHERE = NULL, include_template = FALSE) {
  
  # use SDA to get areasymbol and last updated date to build WSS cache urls
  q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol != 'US'"
  q2 <- ifelse(!is.null(WHERE), paste0(q, " AND (", WHERE, ")"), q)
  sacatalog <- SDA_query(q2)
  
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
    areasymbol, ifelse(include_template, paste0("_soildb_", statecode, "_2003"), ""), "_[", 
    as.Date(saverest, format = "%m/%d/%Y %H:%M:%S"), "].zip"
  )

  unique(res)
}

#' Create a SQLite database or GeoPackage from one or more SSURGO Exports
#'
#' @param filename Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`)
#' @param exdir Path containing containing SSURGO spatial (.shp) and tabular (.txt) files. 
#' @param pattern Character. Optional regular expression to use to filter subdirectories of `exdir`. Default: `NULL` will search all subdirectories for SSURGO export files.#' @param include_spatial Logical. Include spatial data layers in database? Default: `TRUE`. 
#' @param overwrite Logical. Overwrite existing layers? Default `FALSE` will append to existing tables/layers.
#' @param ... Additional arguments passed to `write_sf()` for writing spatial layers.
#' @return Character. Vector of layer/table names in `filename`.
#' @export
#' @examples
#' \dontrun{
#'  downloadSSURGO("areasymbol IN ('CA067', 'CA077', 'CA632')", destdir = "SSURGO_test")
#'  createSSURGO("test.gpkg", "SSURGO_test")
#' }
createSSURGO <- function(filename, exdir, pattern = NULL, overwrite = FALSE, ...) {
  f <- list.files(exdir, recursive = TRUE, pattern = pattern, full.names = TRUE, include.dirs = T)
  
  if (!requireNamespace("sf"))
    stop("package `sf` is required to write spatial datasets to SSURGO SQLite databases", call. = FALSE)
  
  if (!requireNamespace("RSQLite"))
    stop("package `RSQLite` is required to write tabular datasets to SSURGO SQLite databases", call. = FALSE)
  
  # create and add combined vector datasets:
  #   "soilmu_a", "soilmu_l", "soilmu_p", "soilsa_a", "soilsf_l", "soilsf_p" 
  f.shp <- f[grepl(".*\\.shp$", f)]
  shp.grp <- do.call('rbind', strsplit(gsub(".*soil([musfa]{2})_([apl])_([a-z]{2}\\d{3})\\.shp", "\\1;\\2;\\3", f.shp), ";", fixed = TRUE))
  f.shp.grp <- split(f.shp, list(feature = shp.grp[,1], geom = shp.grp[,2]))
  
  lapply(seq_along(f.shp.grp), function(i) {
    lapply(seq_along(f.shp.grp[[i]]), function(j){
      lnm <- gsub(".*(soil[musfa]{2}_[apl])_.*", "\\1", f.shp.grp[[i]][j])
      
      if (overwrite && j == 1) {
        sf::write_sf(sf::read_sf(f.shp.grp[[i]][j]), filename, layer = lnm, overwrite = TRUE, ...)
      } else sf::write_sf(sf::read_sf(f.shp.grp[[i]][j]), filename, layer = lnm, append = TRUE, ...)
      
      # TODO: check/optimize: GPKG vector data includes R*Tree spatial index
      
      NULL
    })
  })

  # create and add combined tabular datasets
  f.txt <- f[grepl(".*\\.txt$", f)]
  txt.grp <- gsub("\\.txt", "", basename(f.txt))
  
  # explicit handling special feature descriptions -> "featdesc" table
  txt.grp[grepl("soilsf_t_", txt.grp)] <- "featdesc"
  
  f.txt.grp <- split(f.txt, txt.grp)
  
  # get table, column, index lookup tables
  mstab <- read.table(f.txt.grp[[which(names(f.txt.grp) == "mstab")]][[1]], sep = "|", stringsAsFactors = FALSE)
  mstab_lut <- mstab$V1
  names(mstab_lut) <- mstab$V5
  mstabcol <- read.table(f.txt.grp[[which(names(f.txt.grp) == "mstabcol")]][[1]], sep = "|", stringsAsFactors = FALSE)
  msidxdet <- read.table(f.txt.grp[[which(names(f.txt.grp) == "msidxdet")]][[1]], sep = "|", stringsAsFactors = FALSE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), filename, loadable.extensions = TRUE)  
  lapply(names(f.txt.grp), function(x) {
    newnames <- mstabcol$V3[mstabcol$V1 == mstab_lut[x]]
    indexPK <- na.omit(msidxdet$V4[msidxdet$V1 == mstab_lut[x] & grepl("PK_", msidxdet$V2)])
    
    d <- try(as.data.frame(data.table::rbindlist(lapply(seq_along(f.txt.grp[[x]]), function(i) {
      if (length(readLines(f.txt.grp[[x]][i]))) {
        
        y <- read.delim(f.txt.grp[[x]][i], sep = "|", stringsAsFactors = FALSE, header = FALSE)
        
        if (length(y) == 1) {
          y <- data.frame(content = y)
        } else colnames(y) <- newnames
        y
      }
    }))), silent = FALSE)
    
    if (is.na(mstab_lut[x])) {
      mstab_lut[x] <- x
    }
    
    if (inherits(d, 'data.frame') && nrow(d) > 0 && !is.na(mstab_lut[x])) {
      # remove repeated records/metadata
      if (ncol(d) > 1) {
        d <- unique(d) 
      }
      
      # write tabular data to file
      try({
        if (overwrite) {
          RSQLite::dbWriteTable(con, mstab_lut[x], d, overwrite = TRUE)
        } else RSQLite::dbWriteTable(con, mstab_lut[x], d, append = TRUE)
      }, silent = FALSE)
      
      # create pkey indices
      if (length(indexPK) > 0) {
        try({
          RSQLite::dbExecute(con, sprintf("CREATE UNIQUE INDEX IF NOT EXISTS %s ON %s (%s)", 
                                          paste0('PK_', mstab_lut[x]), mstab_lut[x], paste0(indexPK, collapse = ",")))
        }, silent = FALSE)
      }
      
      # TODO: other foreign keys/relationships? ALTER TABLE/ADD CONSTRAINT not available in SQLite
      #  the only way to add a foreign key is via CREATE TABLE which means refactoring above two
      #  steps into a single SQL statement (create table with primary and foreign keys)
    }
  })
  
  res <- RSQLite::dbListTables(con)
  RSQLite::dbDisconnect(con)
  invisible(res)
}
