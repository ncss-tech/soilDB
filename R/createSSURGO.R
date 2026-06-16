#' Get SSURGO ZIP files from Web Soil Survey 'Download Soils Data'
#'
#' Download ZIP files containing spatial (ESRI shapefile) and tabular (TXT) files in standard SSURGO
#' format. To specify the Soil Survey Areas you would like to download, use a `WHERE` clause for
#' query of `sacatalog` table, for example: `areasymbol = 'CA067'`, `"areasymbol IN ('CA628',
#' 'CA067')"`, or `areasymbol LIKE 'CT%'`.
#'
#' @param WHERE _character_. A SQL `WHERE` clause expression used to filter records in `sacatalog`
#'   table. Alternately `WHERE` can be any spatial object supported by `SDA_spatialQuery()` for
#'   defining the target extent.
#' @param areasymbols _character_. Character vector of soil survey area symbols e.g. `c("CA067",
#'   "CA077")`. Used in lieu of `WHERE` argument.
#' @param destdir _character_. Directory to download ZIP files into. Default `tempdir()`.
#' @param exdir _character_. Directory to extract ZIP archives into. May be a directory that does
#'   not yet exist. Each ZIP file will extract to a folder labeled with `areasymbol` in this
#'   directory. Default: `destdir`
#' @param include_template _logical_. Include the (possibly state-specific) MS Access template
#'   database? Default: `FALSE`
#' @param include_spatial _logical_ or _character_. Extract spatial data layers from ZIP file?
#'   Default: `TRUE` inserts all spatial tables. If `include_spatial` is a _character_ vector
#'   containing table names, only that set is extracted from the downloaded ZIP files. e.g.
#'   `include_spatial=c("mupolygon", "featpoint")` extracts only the shapefiles (with side car
#'   files) for mapunit polygons and special feature points.
#' @param include_tabular _logical_ or _character_. Extract tabular data from ZIP file? Default:
#'   `TRUE` inserts all tabular tables. If `include_tabular` is a _character_ vector containing
#'   table names, only that set is extracted from the downloaded ZIP files. e.g.
#'   `include_tabular=c("mapunit", "muaggatt")` writes only the `mapunit` and `muaggatt` tables.
#'   Note that special feature descriptions are stored in table `"featdesc"` and metadata for each
#'   soil survey area are stored in `"soil_metadata"` tables.
#' @param db _character_. Either `"SSURGO"` (default; detailed soil map) or `"STATSGO"` (general
#'   soil map).
#' @param extract _logical_. Extract ZIP files to `exdir`? Default: `TRUE`
#' @param LAPPLY.FUN _function_. `lapply()`-like function to use for iteration during `extract`
#'   phase. Only used if `extract=TRUE`. This allows for the `utils::unzip()` operations to be run
#'   in parallel instead of sequential, custom progress reporting, or similar.
#' @param LAPPLY.FUN.ARGS _list_. Optional list of additional arguments to pass to `LAPPLY.FUN`.
#' @param remove_zip _logical_. Remove ZIP files after extracting? Default: `FALSE`
#' @param overwrite _logical_. Overwrite by re-extracting if directory already exists? Default:
#'   `FALSE`
#' @param quiet _logical_. Passed to `curl::curl_download()`.
#'
#' @export
#'
#' @details Pipe-delimited TXT files are found in _/tabular/_ folder extracted from a SSURGO ZIP.
#'   The files are named for tables in the SSURGO schema. There is no header and the files do not
#'   have column names. See the _Soil Data Access Tables and Columns Report_:
#'   \url{https://sdmdataaccess.nrcs.usda.gov/documents/TablesAndColumnsReport.pdf} for details on
#'   tables, column names and metadata including the default sequence of columns used in TXT files.
#'   The function returns a `try-error` if the `WHERE`/`areasymbols` arguments result in
#'
#'   Several ESRI shapefiles are found in the _/spatial/_ folder extracted from a SSURGO ZIP. These
#'   have prefix `soilmu_` (mapunit), `soilsa_` (survey area), `soilsf_` (special features). There
#'   will also be a TXT file with prefix `soilsf_` describing any special features. Shapefile names
#'   then have an `a_` (polygon), `l_` (line), `p_` (point) followed by the soil survey area symbol.
#'   When `db="STATSGO"` the `WHERE` argument is not supported. Allowed `areasymbols` include `"US"`
#'   and two-letter state codes e.g. `"WY"` for the Wyoming general soils map.
#'
#'   As in `createSSURGO()`, the `include_spatial` and `include_tabular` arguments either take a
#'   logical value (default `TRUE`) or a character vector of the specific table names to include.
#'   Note that when used in `downloadSSURGO()` the required metadata files are _always_ extracted to
#'   facilitate mapping to user-facing table names. These arguments allow for customizing the files
#'   that get extracted from ZIP files, not just filtering on file names (as is implemented with
#'   pre-existing `pattern` argument). This can dramatically improve efficiency of extraction and
#'   the overall size of the data in `exdir`. These arguments can be used in conjunction with the
#'   `pattern` argument to fine-tune the files included in the generated snapshot database.
#'
#' @return _character_. Paths to downloaded ZIP files (invisibly). May not exist if `remove_zip =
#'   TRUE`.
#' @seealso [createSSURGO()]
downloadSSURGO <- function(WHERE = NULL,
                           areasymbols = NULL,
                           destdir = tempdir(),
                           exdir = destdir,
                           include_template = FALSE,
                           include_spatial = TRUE,
                           include_tabular = TRUE,
                           db = c('SSURGO', 'STATSGO'),
                           extract = TRUE,
                           LAPPLY.FUN = lapply,
                           LAPPLY.FUN.ARGS = NULL,
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

    UNZIP.FUN <- function(i) {
      if (isTRUE(include_spatial) && isTRUE(include_tabular)) {
        lz <- NULL
      } else {
        lz <- utils::unzip(paths2[i], list = TRUE)$Name
        # need to pre-extract mstab data to map to real column names
        utils::unzip(paths2[i], files = lz[grepl(
          "^(mstab|mdstattabs|MetadataTable|mstabcol|mdstattabcol|MetadataColumnLookup|msidxdet|mdstatidxdet|MetadataIndexDetail)$",
          tools::file_path_sans_ext(basename(lz))
        )], exdir = exdir)

        # explicitly fetch internal function our namespace to support parallel workers
        INV.FUN <- get(".inventory_ssurgo_files", envir = asNamespace("soilDB"))
        inv <- INV.FUN(lz, exdir = exdir, include_spatial = include_spatial, include_tabular = include_tabular)

        lz <- unlist(c(inv$f.shp.sc, inv$f.txt.grp))
      }
      uz <- utils::unzip(paths2[i], files = lz, exdir = exdir)
      if (length(uz) == 0) {
        message(paste('Invalid zipfile:', paths2[i]))
      } else {
        if (!quiet) {
          message("Extracted: ", paths2[i])
        }
      }
    }

    res <- do.call(LAPPLY.FUN, c(list(seq_along(paths2), UNZIP.FUN), LAPPLY.FUN.ARGS))

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
#' @param filename _character_. Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`). Only used when
#'   `con` is not specified by the user.
#' @param exdir  _character_. Path containing containing input SSURGO spatial (.shp) and tabular
#'   (.txt) files, downloaded and extracted by `downloadSSURGO()` or similar.
#' @param conn A _DBIConnection_ object. Default is a `SQLiteConnection` used for writing .sqlite or
#'   .gpkg files. Alternate options are any DBI connection types. When `include_spatial=TRUE`, the
#'   sf package is used to write spatial data to the database.
#' @param pattern  _character_. Optional regular expression to use to filter subdirectories of
#'   `exdir`. Default: `NULL` will search all subdirectories for SSURGO export files.
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
#'   name specified is the grouping variable. Default: `NULL` does no aggregation, giving 1
#'   `POLYGON` feature per delineation. `"mukey"` aggregates all related delineations within a soil
#'   survey area.
#' @param maxruledepth _integer_. Maximum rule depth for `"cointerp"` table. Default `0` includes
#'   only shallowest ratings for smaller database size.
#' @param overwrite _logical_. Overwrite existing layers? Default: `FALSE`
#' @param append _logical_. Append to existing layers? Default: `FALSE`
#' @param header _logical_. Passed to `data.table::fread()` for reading delimited tabular text files. Default: `FALSE`
#' @param sep _character_. Passed to `data.table::fread()`. Default: `"|"`
#' @param na.string _character_. Passed to `data.table::fread()`. Default: `c("", "NA")`
#' @param quote _character_. Passed to `data.table::fread()`. Default: `""`
#' @param quiet _logical_. Suppress messages and other output from database read/write operations?
#' @param ... Additional arguments passed to `sf::write_sf()` for writing spatial layers.
#'
#' @return _character_. Vector of layer/table names in `filename`.
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
                         sep = "|",
                         na.strings = c("", "NA"),
                         quote = "",
                         quiet = TRUE,
                         ...) {

  if ((missing(filename) || length(filename) == 0) && missing(conn)) {
    stop("`filename` should be a path to a .gpkg or .sqlite file to create or append to, or a DBIConnection should be provided via `conn`.")
  }
  
  if (!dir.exists(exdir) ||
      length(list.dirs(exdir)) == 0 || 
      length(list.files(exdir, recursive = TRUE)) == 0) {
    stop(sprintf("SSURGO extraction directory (%s) appears to be empty, check that the path is correct.\n\nIf you want to download new data, run `downloadSSURGO(destdir=%s, ...)` first.",
                 exdir, shQuote(exdir)),
         call. = FALSE)
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

  layer_names <- .get_spatial_layer_names()
  f <- list.files(exdir, recursive = TRUE, full.names = TRUE)
  fdx <- rep(TRUE, length(f))

  if (!is.null(pattern)) {
    fdx <- grepl(pattern, f)
  }
  
  
  if (!quiet) {
    message("Creating SSURGO database from ", exdir, " using pattern ", 
             ifelse(is.null(pattern), "*", pattern), "...")
  
    message(
      "  Output:  ", ifelse(is.null(conn), filename, conn@dbname), "\n",
      "  Spatial: ", paste0(include_spatial, collapse = ", "), "\n",
      "  Tabular: ", paste0(include_tabular, collapse = ", ")
    )
  }
  
  # initialize timing records for this run (collected locally and attached to result)
  write_timings <- list()
  zero_time <- Sys.time()
  
  inv <- .inventory_ssurgo_files(
    files = f[fdx],
    layer_names = layer_names,
    include_spatial = include_spatial,
    include_tabular = include_tabular,
    sep = sep,
    quote = quote,
    na.strings = na.strings,
    header = header
  )

  # inventory method converts partial sets (character vectors) to logical for include_* args
  include_spatial <- inv$include_spatial
  include_tabular <- inv$include_tabular
  
  
  if ((missing(conn) || is.null(conn)) && !IS_GPKG) {

    if (!requireNamespace("RSQLite")) {
      stop("package 'RSQLite' is required (when `conn` is not specified)",
           call. = FALSE)
    }

    conn <- DBI::dbConnect(DBI::dbDriver("SQLite"),
                           filename,
                           loadable.extensions = TRUE)

    # if user did not specify their own connection, close on exit
    on.exit(DBI::dbDisconnect(conn))
  }

  if (nrow(inv$shp.grp) >= 1 && ncol(inv$shp.grp) == 3 && include_spatial) {

    f.shp.grp <- split(inv$f.shp,
                       list(feature = inv$shp.grp[, 1],
                            geom = inv$shp.grp[, 2]),
                       drop = TRUE)

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
            # attempt to capture source file path for SSA extraction (f.shp.grp is in outer scope)
            src_file <- tryCatch(f.shp.grp[[i]][j], error = function(e) NULL)
            if ((i == 1 && j == 1) && isFALSE(append)) {
              rec <- .write_sf_with_log(x = x,
                                 dsn = dsn,
                                 layer = layer,
                                 delete_layer = IS_GPKG || overwrite,
                                 append = FALSE,
                                 file = src_file,
                                 quiet = quiet,
                                 ...)
            } else {
              rec <- .write_sf_with_log(x = x,
                                 dsn = dsn,
                                 layer = layer,
                                 delete_layer = FALSE,
                                 append = TRUE,
                                 file = src_file,
                                 quiet = quiet,
                                 ...)
            }
            invisible(rec)
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
            rec <- .st_write_sf_conn(shp, filename, lnm, j, overwrite)
          } else {
            rec <- .st_write_sf_conn(shp, conn, lnm, j, overwrite)
          }
          
          if (!is.null(rec)) {
            write_timings[[length(write_timings) + 1L]] <- rec
          }
        } 
      } 
    } 
  } 

  if (IS_GPKG) {

    if (!requireNamespace("RSQLite")) {
      stop("package 'RSQLite' is required (when `conn` is not specified)", call. = FALSE)
    }

    conn <- DBI::dbConnect(DBI::dbDriver("SQLite"),
                           filename,
                           loadable.extensions = TRUE)

    # if user did not specify their own connection, close on exit
    on.exit(DBI::dbDisconnect(conn))
  }

  if (include_tabular) {

    if (length(inv$mstabcn) >= 1) {
      mstabcol <- read.delim(inv$mstabcn[1], sep = "|", stringsAsFactors = FALSE, header = header)
    }

    if (length(inv$msidxdn) >= 1) {
      msidxdet <- read.delim(inv$msidxdn[1], sep = "|", stringsAsFactors = FALSE, header = header)
    }

    # build type mapping from SSURGO logicaldatatype metadata
    # mstabcol columns: 1=tabphyname, 2=colsequence, 3=colphyname, 4=collogname,
    #                   5=uomabbrev, 6=logicaldatatype, 7=notnull, 8=fieldsize
    if (length(mstabcol) >= 6) {
      .ssurgo_type_map <- c(
        String = "character", Choice = "character", Vtext = "character",
        `Date/Time` = "character",
        Integer = "integer", Float = "numeric", Boolean = "logical"
      )
    } else {
      .ssurgo_type_map <- NULL
    }
    
    # environment variable (batching is helpful, but larger batch sizes not inherently faster)
    batch_env <- Sys.getenv("R_SOILDB_SSURGO_TABLE_WRITE_BATCH_SIZE", unset = "25")
    if (nzchar(batch_env)) {
      batch_size <- suppressWarnings(as.integer(batch_env))
    }
    
    if (is.na(batch_size) || batch_size <= 0L) {
      warning(
        "Invalid R_SOILDB_SSURGO_TABLE_WRITE_BATCH_SIZE='", batch_env, "', using no batching"
      )
      batch_size <- 1L
    }

    lapply(names(inv$f.txt.grp), function(x) {

      if (!is.null(mstabcol)) {
        newnames <- mstabcol[[3]][mstabcol[[1]] == inv$mstab_lut[x]]
      }

      if (!is.null(msidxdet)) {
        indexPK <- na.omit(msidxdet[[4]][msidxdet[[1]] == inv$mstab_lut[x] & grepl("PK_", msidxdet[[2]])])
        indexDI <- na.omit(msidxdet[[4]][msidxdet[[1]] == inv$mstab_lut[x] & grepl("DI_", msidxdet[[2]])])
      }

      # process files in transaction batches
      files <- inv$f.txt.grp[[x]]
      files_n <- length(files)
      
      if (files_n > 0) {
        
        batch_n <- as.integer(batch_size)
        
        for (start_idx in seq(1, files_n, by = batch_n)) {
          
          end_idx <- min(start_idx + batch_n - 1L, files_n)
          
          tryCatch({
            DBI::dbBegin(conn)
            for (i in seq(start_idx, end_idx)) {
              y <- try({
                .read_delim_tabular(
                  files[i],
                  header = header,
                  newnames = newnames,
                  tablename = inv$mstab_lut[x],
                  mstabcol = mstabcol,
                  type_map = .ssurgo_type_map,
                  maxruledepth = maxruledepth,
                  sep = sep,
                  na.strings = na.strings,
                  quote = quote,
                  quiet = quiet
                )
              }, silent = quiet)

              if (inherits(y, 'try-error') || is.null(y)) {
                if (!quiet) message("File ", files[i], " contains no data or failed to read")
                next
              }

              if (is.na(inv$mstab_lut[x])) next

              try({
                table_name <- inv$mstab_lut[x]
                table_exists <- DBI::dbExistsTable(conn, table_name)
                append_arg <- FALSE
                overwrite_arg <- overwrite

                if (!table_exists) {
                  append_arg <- FALSE
                  overwrite_arg <- FALSE
                } else if (isTRUE(append)) {
                  append_arg <- TRUE
                  overwrite_arg <- FALSE
                } else {
                  append_arg <- FALSE
                  overwrite_arg <- overwrite
                }

                rec <- .write_table_with_log(
                  conn = conn,
                  name = table_name,
                  value = y,
                  overwrite = overwrite_arg,
                  append = append_arg,
                  file = files[i],
                  quiet = quiet
                )

                if (!is.null(rec)) {
                  write_timings[[length(write_timings) + 1L]] <- rec
                }
              }, silent = quiet)
            }
            DBI::dbCommit(conn)
          }, error = function(e) {
            try(DBI::dbRollback(conn), silent = TRUE)
            stop(e)
          })
        }
      }

      if (length(inv$mstab_lut[x]) && is.na(inv$mstab_lut[x])) {
        inv$mstab_lut[x] <- x
      }

      if (length(inv$mstab_lut[x]) && !is.na(inv$mstab_lut[x])) {

        # create pkey indices
        if (!is.null(indexPK) && length(indexPK) > 0) {
          try({
            q <- sprintf("CREATE UNIQUE INDEX IF NOT EXISTS %s ON %s (%s)",
                         paste0('PK_', inv$mstab_lut[x]), inv$mstab_lut[x],
                         paste(indexPK, collapse = ","))
            if (DBI::dbExistsTable(conn, inv$mstab_lut[x]))
              DBI::dbExecute(conn, q)
          }, silent = quiet)
        }

        # create key indices
        if (!is.null(indexDI) && length(indexDI) > 0) {
          for (i in seq_along(indexDI)) {
            try({
              q <- sprintf("CREATE INDEX IF NOT EXISTS %s ON %s (%s)",
                           paste0('DI_', inv$mstab_lut[x]), inv$mstab_lut[x], indexDI[i])
              if (DBI::dbExistsTable(conn, inv$mstab_lut[x]))
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
          if (DBI::dbExistsTable(conn, inv$mstab_lut[x])) {
            try(.gpkg_delete_contents(conn, inv$mstab_lut[x]))
            try(.gpkg_add_contents(conn, inv$mstab_lut[x]))
          }
        }

        # TODO: other foreign keys/relationships? ALTER TABLE/ADD CONSTRAINT not available in SQLite
        #  the only way to add a foreign key is via CREATE TABLE which means refactoring above two
        #  steps into a single SQL statement (create table with primary and foreign keys)
      }
    })
  }
  complete_time <- Sys.time()
  
  if (!quiet) {
    message("Database creation complete in ", format(signif(difftime(complete_time, zero_time, units = "auto"), 3)), ".")
  }
  res <- DBI::dbListTables(conn)

  # attach write timing summary as attribute for programmatic access
  if (length(write_timings) > 0) {
    timings_df <- do.call(rbind, lapply(write_timings, function(r)
      data.frame(
        table = r$table,
        ssa = r$ssa,
        rows = r$rows,
        elapsed = r$elapsed,
        stringsAsFactors = FALSE
      )))
    attr(res, 'write_timings') <- timings_df
  }

  invisible(res)
}

.inventory_ssurgo_files <- function(files,
                                    exdir = NULL,
                                    pattern = NULL,
                                    layer_names = .get_spatial_layer_names(),
                                    include_spatial = TRUE,
                                    include_tabular = TRUE,
                                    sep = "|",
                                    quote = "",
                                    na.strings = c("", "NA"),
                                    header = FALSE) {

  # create and add combined vector datasets:
  #   "soilmu_a", "soilmu_l", "soilmu_p", "soilsa_a", "soilsf_l", "soilsf_p"
  f.shp <- files[grepl(".*\\.shp$", files)]
  shp.grp <- do.call('rbind', strsplit(
    gsub(
      ".*soil([musfa]{2})_([apl])_([a-z]{2}\\d{3}|[a-z]{2})\\.shp",
      "\\1;\\2;\\3",
      f.shp
    ),
    ";",
    fixed = TRUE
  ))

  f.shp.sc <- character(0)
  if (is.character(include_spatial)) {
    idx <- paste0(shp.grp[, 1], "_", shp.grp[, 2]) %in% names(layer_names[layer_names %in% include_spatial])
    shp.grp <- shp.grp[idx, , drop = FALSE]
    f.shp <- f.shp[idx]
    f.shp.sc <- files[grepl(paste0(
    	paste0("soil", shp.grp[, 1], "_", shp.grp[, 2], "_", shp.grp[, 3]),
    	collapse = "|"
    ), files)]
    include_spatial <- TRUE
  }

  # create and add combined tabular datasets
  f.txt <- files[grepl(".*\\.txt$", files)]
  txt.grp <- gsub("\\.txt", "", basename(f.txt))

  # explicit handling special feature descriptions -> "featdesc" table
  txt.grp[grepl("soilsf_t_", txt.grp)] <- "featdesc"
  txt.grp[grepl("soil_metadata_", txt.grp)] <- "soil_metadata"
  txt.first <- unique(txt.grp[grep("^sdv|^md*s|^Metadata", txt.grp)])

  f.txt.grp <- split(f.txt, txt.grp)
  f.txt.grp[txt.first] <- lapply(f.txt.grp[txt.first], .subset, 1)

  # get table, column, index lookup tables
  mstabn <- f.txt.grp[[which(names(f.txt.grp) %in% c("mstab", "mdstattabs", "MetadataTable"))[1]]][[1]]
  mstabcn <- f.txt.grp[[which(names(f.txt.grp) %in% c("mstabcol", "mdstattabcols", "MetadataColumnLookup"))[1]]][[1]]
  msidxdn <- f.txt.grp[[which(names(f.txt.grp) %in% c("msidxdet", "mdstatidxdet", "MetadataIndexDetail"))[1]]][[1]]

  if (length(mstabn) >= 1) {
  	mstab <- read.delim(
  		ifelse(
  			is.null(exdir),
  			yes = mstabn[1],
  			no = file.path(exdir, mstabn[1])
  		),
  		sep = "|",
  		stringsAsFactors = FALSE,
  		header = header
  	)
    mstab_lut <- c(mstab[[1]], "soil_metadata")
    names(mstab_lut) <- c(mstab[[5]], "soil_metadata")
  } else {
    mstab <- NULL
    mstab_lut <- names(f.txt.grp)
    names(mstab_lut) <- names(f.txt.grp)
  }

  if (is.character(include_tabular)) {
    f.txt.grp <- f.txt.grp[names(mstab_lut[mstab_lut %in% include_tabular])]
    include_tabular <- TRUE
  }

  list(
    f.shp = f.shp,
    shp.grp = shp.grp,
    f.shp.sc = f.shp.sc,
    include_spatial = include_spatial,
    f.txt.grp = f.txt.grp,
    txt.first = txt.first,
    include_tabular = include_tabular,
    mstabn = mstabn,
    mstabcn = mstabcn,
    msidxdn = msidxdn,
    mstab = mstab,
    mstab_lut = mstab_lut
  )
}

.get_spatial_layer_names <- function() {
  c(
    `mu_a` = "mupolygon",
    `mu_l` = "muline",
    `mu_p` = "mupoint",
    `sa_a` = "sapolygon",
    `sf_l` = "featline",
    `sf_p` = "featpoint"
  )
}

# helper: coerce columns to schema types from mstabcol metadata
.coerce_ssurgo_types <- function(y, tablename, mstabcol, type_map) {
  if (is.null(type_map) || length(mstabcol) < 6)
    return(y)
  col_meta <- mstabcol[mstabcol[[1]] == tablename, c(3L, 6L), drop = FALSE]
  for (j in seq_len(nrow(col_meta))) {
    col  <- col_meta[[1L]][j]
    rtyp <- type_map[col_meta[[2L]][j]]
    if (is.na(rtyp) || !col %in% names(y))
      next
    y[[col]] <- switch(
      rtyp,
      character = as.character(y[[col]]),
      integer   = suppressWarnings(as.integer(as.character(y[[col]]))),
      numeric   = suppressWarnings(as.numeric(as.character(y[[col]]))),
      logical   = as.logical(y[[col]])
    )
  }
  y
}

# helper: fast delimited text reader
.read_delim_tabular <- function(file,
                                header,
                                newnames,
                                tablename,
                                mstabcol,
                                type_map,
                                maxruledepth,
                                sep = "|",
                                na.strings = c("", "NA"),
                                quote = "",
                                quiet) {
  dt <- suppressWarnings(data.table::fread(
    file,
    sep = sep,
    header = header,
    na.strings = na.strings,
    showProgress = FALSE,
    data.table = FALSE
  ))
  
  if (length(dt) == 1) {
    if (grepl("soil_metadata", file)) {
      y <- data.frame(
        areasymbol = toupper(gsub(
          ".*soil_metadata_(.*)\\.txt", "\\1", file
        )),
        content = paste(dt[[1]], collapse = "\n"),
        stringsAsFactors = FALSE
      )
      return(y)
    } else {
      y <- data.frame(content = dt, stringsAsFactors = FALSE)
      return(y)
    }
  }
  
  y <- dt
  if (!is.null(mstabcol) && !header && !is.null(newnames) && length(y) > 0) {
    colnames(y) <- newnames
  }
  if (length(y) == 0) {
    return(try(stop("failed to read"), silent = TRUE))
  }
  y <- .coerce_ssurgo_types(y, tablename, mstabcol, type_map)
  
  # cointerp is generally large; filter early by ruledepth by defaykt
  if (!is.null(maxruledepth) &&
      tablename == "cointerp" &&
      "ruledepth" %in% colnames(y)) {
    y <- y[y$ruledepth <= maxruledepth, , drop = FALSE]
  }
  
  if ("musym" %in% colnames(y)) {
    y$musym <- as.character(y$musym)
  }
  
  y
}

.write_table_with_log <- function(conn,
                                 name,
                                 value,
                                 overwrite = FALSE,
                                 append = FALSE,
                                 file = NULL,
                                 quiet = TRUE) {
  
 # attempt to extract SSA identifier from file path if present
 ssa <- NA_character_
 if (!is.null(file) && nzchar(file)) {
   try(silent = TRUE, {
     ssa <- toupper(basename(.deep_dirname(file, ifelse(grepl("soil_metadata", file), 1, 2))))
   })
 }

 start_time <- Sys.time()

 # environment variable (at this scale chunk size mostly negligible)
 chunk_size <- as.integer(Sys.getenv("R_SOILDB_SSURGO_ROW_CHUNK_SIZE", "100000"))
 
 if (is.na(chunk_size) || chunk_size <= 0L) {
   warning(
     "Invalid R_SOILDB_SSURGO_ROW_CHUNK_SIZE='", chunk_size, "', using 100,000 row batches"
   )
   chunk_size <- 100000L
 }

 res <- tryCatch({
   rows_written <- NA_integer_
   if (isTRUE(append) && is.data.frame(value)) {
     n <- nrow(value)

     if (n == 0) {
       rows_written <- 0L
     } else {
       if (n <= chunk_size) {
         DBI::dbAppendTable(conn, name, value)
       } else {
         starts <- seq(1, n, by = chunk_size)
         for (s in starts) {
           e <- min(s + chunk_size - 1L, n)
           DBI::dbAppendTable(conn, name, value[s:e, , drop = FALSE])
         }
       }
       rows_written <- n
     }
   } else {
     # not an append or not a data.frame: write in one shot
     DBI::dbWriteTable(conn, name, value, overwrite = overwrite)
     rows_written <- ifelse(is.data.frame(value), nrow(value), NA_integer_)
   }

   end_time <- Sys.time()
   elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

   if (!quiet) {
     message(
       sprintf(
         "[%s] WRITE table=%s ssa=%s rows=%s elapsed=%.3f",
         .format_iso8601(end_time),
         as.character(name),
         ifelse(is.na(ssa) || length(ssa) == 0, "NA", ssa),
         ifelse(is.numeric(rows_written), rows_written, NA_integer_),
         elapsed
       )
     )
   }
   
   flush.console()

   record <- list(
     table = as.character(name),
     ssa = ifelse(is.na(ssa) || length(ssa) == 0, NA_character_, as.character(ssa)),
     rows = ifelse(is.numeric(rows_written), as.integer(rows_written), NA_integer_),
     elapsed = elapsed
   )

   invisible(record)
 }, error = function(e) {
   end_time <- Sys.time()
   elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
   if (!quiet) {
     message(
       sprintf(
         "[%s] ERROR write table=%s ssa=%s elapsed=%.3f error=%s",
         .format_iso8601(end_time),
         as.character(name),
         ifelse(is.na(ssa) || length(ssa) == 0, "NA", ssa),
         elapsed,
         conditionMessage(e)
       )
     )
   }   
   flush.console()
   stop(e)
 })
 
 res
}


.write_sf_with_log <- function(x,
                               dsn,
                               layer,
                               delete_layer = FALSE,
                               append = FALSE,
                               file = NULL,
                               quiet = TRUE,
                               ...) {
  ssa <- NA_character_
  if (!is.null(file) && nzchar(file)) {
    try(silent = TRUE, {
      ssa <- toupper(basename(.deep_dirname(file, 2)))
    })
  }

  start_time <- Sys.time()

  res <- tryCatch({
    # delegate to sf::write_sf with the same args used currently
    if (!isTRUE(append)) {
      sf::write_sf(x,
                   dsn = dsn,
                   layer = layer,
                   delete_layer = delete_layer,
                   ...)
    } else {
      sf::write_sf(x,
                   dsn = dsn,
                   layer = layer,
                   append = TRUE,
                   ...)
    }
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    features <- ifelse(inherits(x, 'data.frame'), nrow(x), NA_integer_)

    # compact single-line log
    if (!quiet) {
      message(
        sprintf(
          "[%s] WRITE sf=%s ssa=%s features=%s elapsed=%.3f",
          .format_iso8601(end_time),
          as.character(layer),
          ifelse(is.na(ssa) || length(ssa) == 0, "NA", ssa),
          ifelse(is.numeric(features), features, NA_integer_),
          elapsed
        )
      )
    }
    flush.console()

    record <- list(
      table = as.character(layer),
      ssa = ifelse(is.na(ssa) || length(ssa) == 0, NA_character_, as.character(ssa)),
      rows = ifelse(is.numeric(features), as.integer(features), NA_integer_),
      elapsed = elapsed
    )
    invisible(record)
  }, error = function(e) {
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    if (!quiet) {
      message(
        sprintf(
          "[%s] ERROR write sf=%s ssa=%s elapsed=%.3f error=%s",
          .format_iso8601(end_time),
          as.character(layer),
          ifelse(is.na(ssa) || length(ssa) == 0, "NA", ssa),
          elapsed,
          conditionMessage(e)
        )
      )
    }
    flush.console()
    stop(e)
  })

  res
}


# print summary of write timings collected during run
.ssurgo_print_write_summary <- function(res_obj = NULL, top_n = 30) {
  if (!is.null(res_obj) &&
      !is.null(attr(res_obj, 'write_timings'))) {
    df <- attr(res_obj, 'write_timings')
  } else {
    message(
      'No write timings recorded.'
    )
    return(invisible(NULL))
  }
  df$elapsed <- as.numeric(df$elapsed)

  dt <- data.table::as.data.table(df)
  agg_dt <- dt[, .(
    n = .N,
    total_elapsed = sum(elapsed),
    mean = mean(elapsed),
    sd = if (.N > 1) sd(elapsed) else as.numeric(NA),
    min = min(elapsed),
    max = max(elapsed)
  ), by = table]

  data.table::setorder(agg_dt, -total_elapsed)
  print(head(agg_dt, top_n))
  invisible(as.data.frame(agg_dt))
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
