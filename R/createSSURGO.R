#' Get SSURGO ZIP files from Web Soil Survey 'Download Soils Data'
#'
#' Download ZIP files containing spatial (ESRI shapefile) and tabular (TXT) files with standard
#' SSURGO format; optionally including the corresponding SSURGO Template Database with
#' `include_template=TRUE`.
#'
#' To specify the Soil Survey Areas you would like to obtain data you use a `WHERE` clause for query
#' of `sacatalog` table such as `areasymbol = 'CA067'`, `"areasymbol IN ('CA628', 'CA067')"` or
#' `areasymbol LIKE 'CT%'`.
#'
#' @param WHERE _character_. A SQL `WHERE` clause expression used to filter records in `sacatalog` table.
#'   Alternately `WHERE` can be any spatial object supported by `SDA_spatialQuery()` for defining
#'   the target extent.
#' @param areasymbols _character_. Character vector of soil survey area symbols e.g. `c("CA067", "CA077")`. Used
#'   in lieu of `WHERE` argument.
#' @param destdir _character_. Directory to download ZIP files into. Default `tempdir()`.
#' @param exdir _character_. Directory to extract ZIP archives into. May be a directory that does not yet exist.
#'   Each ZIP file will extract to a folder labeled with `areasymbol` in this directory. Default:
#'   `destdir`
#' @param include_template _logical_. Include the (possibly state-specific) MS Access template database?
#'   Default: `FALSE`
#' @param db _character_. Either `"SSURGO"` (default; detailed soil map) or `"STATSGO"` (general soil map).
#' @param extract _logical_. Extract ZIP files to `exdir`? Default: `TRUE`
#' @param remove_zip _logical_. Remove ZIP files after extracting? Default: `FALSE`
#' @param overwrite _logical_. Overwrite by re-extracting if directory already exists? Default:
#'   `FALSE`
#' @param quiet _logical_. Passed to `curl::curl_download()`.
#' 
#' @details When `db="STATSGO"` the `WHERE` argument is not supported. Allowed `areasymbols` include
#' `"US"` and two-letter state codes e.g. `"WY"` for the Wyoming general soils map.
#'
#' @export
#'
#' @details Pipe-delimited TXT files are found in _/tabular/_ folder extracted from a SSURGO ZIP.
#'   The files are named for tables in the SSURGO schema. There is no header / the files do not have
#'   column names. See the _Soil Data Access Tables and Columns Report_:
#'   \url{https://sdmdataaccess.nrcs.usda.gov/documents/TablesAndColumnsReport.pdf} for details on
#'   tables, column names and metadata including the default sequence of columns used in TXT files.
#'   The function returns a `try-error` if the `WHERE`/`areasymbols` arguments result in
#'
#'   Several ESRI shapefiles are found in the _/spatial/_ folder extracted from a SSURGO ZIP. These
#'   have prefix `soilmu_` (mapunit), `soilsa_` (survey area), `soilsf_` (special features). There
#'   will also be a TXT file with prefix `soilsf_` describing any special features. Shapefile names
#'   then have an `a_` (polygon), `l_` (line), `p_` (point) followed by the soil survey area symbol.
#'
#' @return _character_. Paths to downloaded ZIP files (invisibly). May not exist if `remove_zip =
#'   TRUE`.
#' @seealso [createSSURGO()]
downloadSSURGO <- function(WHERE = NULL, 
                           areasymbols = NULL,
                           destdir = tempdir(), 
                           exdir = destdir, 
                           include_template = FALSE,
                           db = c('SSURGO', 'STATSGO'),
                           extract = TRUE, 
                           remove_zip = FALSE,
                           overwrite = FALSE,
                           quiet = FALSE) {
  
  db <- match.arg(toupper(db), c('SSURGO', 'STATSGO'))
  
  if (!is.null(WHERE) && db == "STATSGO") {
    stop('custom WHERE clause not supported with db="STATSGO"', call. = FALSE)
  }
  
  if (!is.null(areasymbols) && db == "STATSGO") {
    WHERE <- areasymbols
  }
  
  if (is.null(WHERE) && is.null(areasymbols)) {
    stop('must specify either `WHERE` or `areasymbols` argument', call. = FALSE)
  }
    
  if (is.null(WHERE) && !is.null(areasymbols)) {
    WHERE <- sprintf("areasymbol IN %s", format_SQL_in_statement(areasymbols))
  }
  
  if (!is.character(WHERE)) {
    # attempt passing WHERE to SDA_spatialQuery
    res <- suppressMessages(SDA_spatialQuery(WHERE, what = 'areasymbol'))
    WHERE <- paste("areasymbol IN", format_SQL_in_statement(res$areasymbol))
  }
  
  # make WSS download URLs from areasymbol, template, date
  urls <- .make_WSS_download_url(WHERE, include_template = include_template, db = db)
  
  if (inherits(urls, 'try-error')) {
    message(urls[1])
    return(invisible(urls))
  }
  
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }
  
  # download files
  for (i in seq_along(urls)) {
    destfile <- file.path(destdir, basename(urls[i]))
    if (!file.exists(destfile)) {
      try(curl::curl_download(urls[i], destfile = destfile, quiet = quiet, mode = "wb", handle = .soilDB_curl_handle()), silent = quiet)
    }
  }
  
  paths <- list.files(destdir, pattern = "\\.zip$", full.names = TRUE)
  paths2 <- paths[grep(".*wss_(SSA|gsmsoil)_(.*)_.*", paths)]
  
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
      ssa <- gsub(".*wss_SSA_(.*)_.*", "\\1", paths2[i])
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
 
#' Create a database from SSURGO Exports
#'
#' The following database types are tested and fully supported:
#'  - SQLite or Geopackage
#'  - DuckDB
#'  - Postgres or PostGIS
#'
#' In theory any other DBI-compatible data source can be used for output. See `conn` argument. If
#' you encounter issues using specific DBI connection types, please report in the soilDB issue
#' tracker.
#'
#' @param filename _character_. Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`). Only used when `con` is not
#'   specified by the user.
#' @param exdir  _character_. Path containing containing input SSURGO spatial (.shp) and tabular (.txt) files,
#'   downloaded and extracted by `downloadSSURGO()` or similar.
#' @param conn A _DBIConnection_ object. Default is a `SQLiteConnection` used for writing .sqlite or
#'   .gpkg files. Alternate options are any DBI connection types. When `include_spatial=TRUE`, the
#'   sf package is used to write spatial data to the database.
#' @param pattern  _character_. Optional regular expression to use to filter subdirectories of `exdir`.
#'   Default: `NULL` will search all subdirectories for SSURGO export files.
#' @param include_spatial _logical_ or _character_. Include spatial data layers in database?
#'   Default: `TRUE` inserts all spatial tables. If `include_spatial` is a _character_ vector
#'   containing table names, only that set are written to file. e.g. `include_spatial=c("mupolygon",
#'   "featpoint")` writes only the mapunit polygons and special feature points.
#' @param include_tabular _logical_ or _character_. Include tabular data layers in database?
#'   Default: `TRUE` inserts all tabular tables. If `include_tabular` is a _character_ vector
#'   containing table names, only that set are written to file. e.g. `include_tabular=c("mapunit",
#'   "muaggatt")` writes only the `mapunit` and `muaggatt` tables. Note that special feature
#'   descriptions are stored in table `"featdesc"` and metadata for each soil survey area are stored
#'   in `"soil_metadata"` tables.
#' @param dissolve_field _character_. Dissolve geometries to create MULTIPOLYGON features? Column
#'   name
#'   specified is the grouping variable. Default: `NULL` does no aggregation, giving 1 `POLYGON`
#'   feature per delineation. `"mukey"` aggregates all related delineations within a soil survey
#'   area.
#' @param maxruledepth _integer_. Maximum rule depth for `"cointerp"` table. Default `0` includes only
#'   shallowest ratings for smaller database size.
#' @param overwrite _logical_. Overwrite existing layers? Default: `FALSE`
#' @param append _logical_. Append to existing layers? Default: `FALSE`
#' @param header _logical_. Passed to `read.delim()` for reading pipe-delimited (`|`) text files
#'   containing tabular data.
#' @param quiet _logical_. Suppress messages and other output from database read/write operations?
#' @param ... Additional arguments passed to `write_sf()` for writing spatial layers.
#'
#' @return Character. Vector of layer/table names in `filename`.
#' @seealso [downloadSSURGO()]
#' @export
#' @examples
#' \dontrun{
#'  downloadSSURGO("areasymbol IN ('CA067', 'CA077', 'CA632')", destdir = "SSURGO_test")
#'  createSSURGO("test.gpkg", "SSURGO_test")
#' }
createSSURGO <- function(filename = NULL,
                         exdir,
                         conn = NULL, 
                         pattern = NULL,
                         include_spatial = TRUE,
                         include_tabular = TRUE,
                         dissolve_field = NULL,
                         maxruledepth = 0,
                         overwrite = FALSE,
                         append = FALSE,
                         header = FALSE,
                         quiet = TRUE,
                         ...) {
  
  if ((missing(filename) || length(filename) == 0) && missing(conn)) {
    stop("`filename` should be a path to a .gpkg or .sqlite file to create or append to, or a DBIConnection should be provided via `conn`.")
  }
  
  if (missing(conn) || is.null(conn)) {
    # delete existing file if overwrite=TRUE; does _not_ apply to DBIConnection
    if (file.exists(filename)) {
      if (isTRUE(overwrite) && isFALSE(append)) {
        file.remove(filename)      
      } else if (isTRUE(overwrite) && isTRUE(append)) {
        stop("Both overwrite=TRUE and append=TRUE; set only one argument to TRUE", call. = FALSE)
      } else if (isFALSE(overwrite) && isFALSE(append)) {
        stop("File '", filename,"' exists and overwrite=FALSE; use append=TRUE to append to an existing file", call. = FALSE)
      }
    }
  }
  
  # DuckDB has special spatial format, so it gets custom handling for
  IS_DUCKDB <- inherits(conn, "duckdb_connection")
  
  if (inherits(conn, 'SQLiteConnection')) {
    IS_GPKG <- grepl("\\.gpkg$", conn@dbname, ignore.case = TRUE)[1]
    filename <- conn@dbname
  } else {
    IS_GPKG <- grepl("\\.gpkg$", filename, ignore.case = TRUE)[1]
    if (is.na(IS_GPKG)) {
      IS_GPKG <- FALSE
    }
  }
  
  if (!IS_DUCKDB && !requireNamespace("sf")) {
    stop("package `sf` is required to write spatial datasets to DBI data sources", call. = FALSE)
  } 
  
  f <- list.files(exdir, recursive = TRUE, pattern = pattern, full.names = TRUE)
  
  # create and add combined vector datasets:
  #   "soilmu_a", "soilmu_l", "soilmu_p", "soilsa_a", "soilsf_l", "soilsf_p" 
  f.shp <- f[grepl(".*\\.shp$", f)]
  shp.grp <- do.call('rbind', strsplit(gsub(".*soil([musfa]{2})_([apl])_([a-z]{2}\\d{3}|[a-z]{2})\\.shp", "\\1;\\2;\\3", f.shp), ";", fixed = TRUE))
  
  layer_names <- c(`mu_a` = "mupolygon", `mu_l` = "muline",   `mu_p` = "mupoint", 
                   `sa_a` = "sapolygon", `sf_l` = "featline", `sf_p` = "featpoint")
  
  if (is.character(include_spatial)) {
    idx <- paste0(shp.grp[, 1], "_", shp.grp[, 2]) %in% names(layer_names[layer_names %in% include_spatial])
    shp.grp <- shp.grp[idx, ]
    f.shp <- f.shp[idx]
    include_spatial <- TRUE
  }
  
  if (missing(conn) || is.null(conn)) {
    
    if (!requireNamespace("RSQLite")) {
      stop("package 'RSQLite' is required (when `conn` is not specified)", call. = FALSE)
    }
    
    conn <- DBI::dbConnect(DBI::dbDriver("SQLite"),
                           filename, 
                           loadable.extensions = TRUE)
    
    # if user did not specify their own connection, close on exit
    on.exit(DBI::dbDisconnect(conn))
  } 
  
  if (nrow(shp.grp) >= 1 && ncol(shp.grp) == 3 && include_spatial) {
    f.shp.grp <- split(f.shp, list(feature = shp.grp[, 1], geom = shp.grp[, 2]), drop = TRUE)
    
    if (IS_DUCKDB) {
      DBI::dbExecute(conn, "INSTALL spatial; LOAD spatial;")
    }
    
    for (i in seq_along(f.shp.grp)) {
      for (j in seq_along(f.shp.grp[[i]])) {
        lnm <- layer_names[match(gsub(".*soil([musfa]{2}_[apl])_.*", "\\1", f.shp.grp[[i]][j]),
                                 names(layer_names))]
        if (IS_DUCKDB) {
          if (j == 1 && isFALSE(append)) {
            DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s;", lnm))
            DBI::dbExecute(conn, sprintf("CREATE TABLE %s AS SELECT * FROM ST_Read('%s');",
                                lnm, f.shp.grp[[i]][j]))
          } else {
            DBI::dbExecute(conn, sprintf("INSERT INTO %s SELECT * FROM ST_Read('%s');",
                                             lnm, f.shp.grp[[i]][j]))
          }
        } else {
          shp <- sf::read_sf(f.shp.grp[[i]][j])
          
          colnames(shp) <- tolower(colnames(shp))
          sf::st_geometry(shp) <- "geometry"
          
          .st_write_sf_conn <-  function(x, dsn, layer, j, overwrite) {
             if (j == 1 && isFALSE(append)) {
              sf::write_sf(x, dsn = dsn, layer = layer, delete_layer = TRUE, ...)
            } else {
              sf::write_sf(x, dsn = dsn, layer = layer, append = TRUE, ...)
            }
          }
          
          # dissolve on dissolve_field
          # TODO: add nationalmusym to spatial layer?
          if (!is.null(dissolve_field) && dissolve_field %in% colnames(shp)) {
            l <- list(shp[[dissolve_field]])
            names(l) <- dissolve_field
            if (nrow(shp) > 0) {
              shp <- aggregate(shp, by = l, FUN = function(y) {
                yy <- unique(y)
                if (length(yy) > 1) {
                  res <- NA
                  mode(res) <- mode(y)
                  return(res)
                }
                y[1]
              }, do_union = TRUE)
              
              shp[[paste0(dissolve_field, ".1")]] <- NULL
              
              destgeom <- "MULTIPOLYGON"
              if (any(sf::st_geometry_type(shp) %in% c("POINT", "MULTIPOINT"))) {
                destgeom <- "MULTIPOINT"
              } else if (any(sf::st_geometry_type(shp) %in% c("LINESTRING", "MULTILINESTRING"))) {
                destgeom <- "MULTILINESTRING"
              }
              
              shp <- sf::st_cast(shp, destgeom)
            }
          }
          
          if (IS_GPKG && missing(conn)) {
            # writing to SQLiteConnection fails to create proper gpkg_contents entries
            # so use the path for GPKG only
            .st_write_sf_conn(shp, filename, lnm, j)
          } else {
            .st_write_sf_conn(shp, conn, lnm, j)
          }
        }
        NULL
      }
    }
  }
  
  # create and add combined tabular datasets
  f.txt <- f[grepl(".*\\.txt$", f)]
  txt.grp <- gsub("\\.txt", "", basename(f.txt))
  
  # explicit handling special feature descriptions -> "featdesc" table
  txt.grp[grepl("soilsf_t_", txt.grp)] <- "featdesc"
  txt.grp[grepl("soil_metadata_", txt.grp)] <- "soil_metadata"
  txt.first <- unique(txt.grp[grep("^sdv|^ms", txt.grp)])
  
  f.txt.grp <- split(f.txt, txt.grp)
  f.txt.grp[txt.first] <- lapply(f.txt.grp[txt.first], .subset, 1)
  
  # get table, column, index lookup tables
  mstabn <- f.txt.grp[[which(names(f.txt.grp) %in% c("mstab", "mdstattabs", "MetadataTable"))[1]]][[1]]
  mstabcn <- f.txt.grp[[which(names(f.txt.grp) %in% c("mstabcol", "mdstattabcols", "MetadataColumnLookup"))[1]]][[1]]
  msidxdn <- f.txt.grp[[which(names(f.txt.grp) %in% c("msidxdet", "mdstatidxdet", "MetadataIndexDetail"))[1]]][[1]]
  
  if (length(mstabn) >= 1) {
    mstab <- read.delim(mstabn[1], sep = "|", stringsAsFactors = FALSE, header = header)
    mstab_lut <- c(mstab[[1]], "soil_metadata")
    names(mstab_lut) <- c(mstab[[5]], "soil_metadata")
  } else {
    mstab_lut <- names(f.txt.grp)
    names(mstab_lut) <- names(f.txt.grp)
  }
  
  if (is.character(include_tabular)) {
    f.txt.grp <- f.txt.grp[names(mstab_lut[mstab_lut %in% include_tabular])]
    include_tabular <- TRUE
  }
  
  if (include_tabular) {

    if (length(mstabcn) >= 1) {
      mstabcol <- read.delim(mstabcn[1], sep = "|", stringsAsFactors = FALSE, header = header)
    }
    
    if (length(msidxdn) >= 1) {
      msidxdet <- read.delim(msidxdn[1], sep = "|", stringsAsFactors = FALSE, header = header)
    }
    
    lapply(names(f.txt.grp), function(x) {
      
      if (!is.null(mstabcol)) {
        newnames <- mstabcol[[3]][mstabcol[[1]] == mstab_lut[x]]
      }
      
      if (!is.null(msidxdet)) {
        indexPK <- na.omit(msidxdet[[4]][msidxdet[[1]] == mstab_lut[x] & grepl("PK_", msidxdet[[2]])])
        indexDI <- na.omit(msidxdet[[4]][msidxdet[[1]] == mstab_lut[x] & grepl("DI_", msidxdet[[2]])])
      }
      
      d <- try(lapply(seq_along(f.txt.grp[[x]]), function(i) {
          # message(f.txt.grp[[x]][i])
          y <- try(read.delim(f.txt.grp[[x]][i], sep = "|", stringsAsFactors = FALSE, header = header), silent = TRUE)
          
          if (inherits(y, 'try-error')) {
            if (!quiet) {
              message("File ", f.txt.grp[[x]][i], " contains no data")
            }
            return(NULL)
          } else if (length(y) == 1) {
            if (grepl("soil_metadata", f.txt.grp[[x]][i])) {
              y <- data.frame(
                areasymbol = toupper(gsub(".*soil_metadata_(.*)\\.txt", "\\1", f.txt.grp[[x]][i])),
                content = paste(y[[1]], collapse = "\n")
              )
            } else {
              y <- data.frame(content = y)
            }
          } else {
            if (!is.null(mstab) && !header) { # preserve headers if present 
              colnames(y) <- newnames
            }
          }
          
          if (is.na(mstab_lut[x])) {
            # readme, version
            return(NULL)
          }
          
          # remove deeper rules from cointerp for smaller DB size
          # most people only use depth==0 (default)
          if (mstab_lut[x] == "cointerp" && !is.null(maxruledepth)) {
            y <- y[y$ruledepth <= maxruledepth, ]
          }
          
          if ("musym" %in% colnames(y)) {
            y$musym <- as.character(y$musym)
          }
          
          try({
            if (i == 1 && isFALSE(append)) {
              DBI::dbWriteTable(conn, mstab_lut[x], y, overwrite = TRUE)
            } else {
              DBI::dbWriteTable(conn, mstab_lut[x], y, append = TRUE)
            }
          }, silent = quiet)
      }), silent = quiet)
      
      if (length(mstab_lut[x]) && is.na(mstab_lut[x])) {
        mstab_lut[x] <- x
      }
      
      if (length(mstab_lut[x]) && !is.na(mstab_lut[x])) {
        
        # create pkey indices
        if (!is.null(indexPK) && length(indexPK) > 0) {
          try({
            q <- sprintf("CREATE UNIQUE INDEX IF NOT EXISTS %s ON %s (%s)", 
                         paste0('PK_', mstab_lut[x]), mstab_lut[x], 
                         paste(indexPK, collapse = ","))
            if (DBI::dbExistsTable(conn, mstab_lut[x]))
              DBI::dbExecute(conn, q)
          }, silent = quiet)
        }
        
        # create key indices
        if (!is.null(indexDI) && length(indexDI) > 0) {
          for (i in seq_along(indexDI)) {
            try({
              q <- sprintf("CREATE INDEX IF NOT EXISTS %s ON %s (%s)", 
                           paste0('DI_', mstab_lut[x]), mstab_lut[x], indexDI[i])
              if (DBI::dbExistsTable(conn, mstab_lut[x]))
                DBI::dbExecute(conn, q)
            }, silent = quiet)
          }
        }
        
        # for GPKG output, add gpkg_contents (metadata for features and attributes)
        if (IS_GPKG) {
          if (!.gpkg_has_contents(conn)) {
            # if no spatial data inserted, there will be no gpkg_contents table initally
            try(.gpkg_create_contents(conn))
          }
          # update gpkg_contents table entry
          if (DBI::dbExistsTable(conn, mstab_lut[x])) {
            try(.gpkg_delete_contents(conn, mstab_lut[x]))
            try(.gpkg_add_contents(conn, mstab_lut[x]))
          }
        }
        
        # TODO: other foreign keys/relationships? ALTER TABLE/ADD CONSTRAINT not available in SQLite
        #  the only way to add a foreign key is via CREATE TABLE which means refactoring above two
        #  steps into a single SQL statement (create table with primary and foreign keys)
      }
    })
  }
  
  res <- DBI::dbListTables(conn)
  invisible(res)
}

## From https://github.com/brownag/gpkg -----

#' Add, Remove, Update and Create `gpkg_contents` table and records
#' @description `gpkg_add_contents()`: Add a record to `gpkg_contents`
#' @param con A _geopackage_
#' @param table_name Name of table to add or remove record for in _gpkg_contents_
#' @param description Default `""`
#' @param template Default `NULL` uses global EPSG:4326 with bounds -180,-90:180,90
#' @return Result of `RSQLite::dbExecute()`
#' @noRd
#' @keywords internal
.gpkg_add_contents <- function(con, table_name, description = "", template = NULL) {
  
  stopifnot(requireNamespace("RSQLite"))
  
  if (!missing(template) &&
      !is.null(template) &&
      is.list(template) &&
      all(c("ext", "srsid") %in% names(template))) {
    ex <- template$ext
    cr <- as.integer(template$srsid)
  } else {
    ex <- c(-180, -90, 180, 90)
    cr <- 4326
  }
  
  # append to gpkg_contents
  RSQLite::dbExecute(con,
                    paste0(
                      "INSERT INTO gpkg_contents (table_name, data_type, identifier, 
                                  description, last_change,
                                  min_x, min_y, max_x, max_y, srs_id) 
       VALUES ('",
       table_name ,
       "', 'attributes', '",
       table_name,
       "', '",
       description,
       "','",
       strftime(Sys.time(), '%Y-%m-%dT%H:%M:%OS3Z'),
       "', ", ex[1], ", ", ex[2], ", ",
       ex[3], ", ", ex[4], ", ",
       cr, "
                      );"
                    )
  )
}

#' @description `.gpkg_delete_contents()`: Delete a record from `gpkg_contents` based on table name
#' @noRd
#' @keywords internal
.gpkg_delete_contents <- function(con, table_name) {
  stopifnot(requireNamespace("RSQLite"))
  RSQLite::dbExecute(con, paste0("DELETE FROM gpkg_contents WHERE table_name = '", table_name, "'"))
}

#' @description `.gpkg_has_contents()`: Determine if a database has table named `"gpkg_contents"` 
#' @noRd
#' @keywords internal
.gpkg_has_contents <- function(con) {
  stopifnot(requireNamespace("RSQLite"))
  isTRUE("gpkg_contents" %in% RSQLite::dbListTables(con))
}

#' @description `.gpkg_has_contents()`: Determine if a database has table named `"gpkg_contents"` 
#' @noRd
#' @keywords internal
.gpkg_create_contents <- function(con) {
    stopifnot(requireNamespace("RSQLite"))
    q <- "CREATE TABLE gpkg_contents (
      table_name TEXT NOT NULL PRIMARY KEY,
      data_type TEXT NOT NULL,
      identifier TEXT UNIQUE,
      description TEXT DEFAULT '',
      last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
      min_x DOUBLE,
      min_y DOUBLE,
      max_x DOUBLE,
      max_y DOUBLE,
      srs_id INTEGER,
      CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id)
    )"
    
    if (!.gpkg_has_contents(con)) {
      RSQLite::dbExecute(con, q)
    } else return(1)
}
