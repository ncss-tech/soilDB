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
#' @param destdir _character_. Directory to download ZIP files into. Default `NULL` uses the
#'   persistent soilDB Web Soil Survey cache.
#' @param exdir _character_. Directory to extract ZIP archives into. May be a directory that does
#'   not yet exist. Each ZIP file will extract to a folder labeled with `areasymbol` in this
#'   directory. Default: `destdir`, or when `destdir` is `NULL`, the fiscal-year directory containing
#'   the cached ZIP files. When extraction is enabled (`extract = TRUE`) and the target ZIP files
#'   span multiple fiscal-year cache directories, an explicit `exdir` must be provided.
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
#' @param force _logical_. Force re-download of ZIP files even when a cached copy exists. Default:
#'   `FALSE`
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
#'   When `destdir` is omitted, ZIP files are stored in the persistent soilDB Web Soil Survey cache.
#'   Cache paths are organized by fiscal year, and the cache policy also distinguishes template and
#'   non-template archives when choosing the ZIP to reuse. Use `list_WSS_cache()` or `clear_WSS_cache()` to inspect or
#'   clean the cache.
#'
#' @return _character_. Paths to downloaded ZIP files (invisibly). May not exist if `remove_zip =
#'   TRUE`.
#' @seealso [createSSURGO()], [list_WSS_cache()], [clear_WSS_cache()]
downloadSSURGO <- function(WHERE = NULL,
                           areasymbols = NULL,
                           destdir = NULL,
                           exdir = NULL,
                           include_template = FALSE,
                           include_spatial = TRUE,
                           include_tabular = TRUE,
                           db = c('SSURGO', 'STATSGO'),
                           extract = TRUE,
                           LAPPLY.FUN = lapply,
                           LAPPLY.FUN.ARGS = NULL,
                           remove_zip = FALSE,
                           overwrite = FALSE,
                           quiet = FALSE,
                           force = FALSE) {

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
  urls <- try({
    .make_WSS_download_url(WHERE, include_template = include_template, db = db)
  }, silent = TRUE)

  if (inherits(urls, 'try-error')) {
    stop("Failed to resolve remote WSS metadata for requested SSURGO download.", call. = FALSE)
  }

  cache_mode <- is.null(destdir)
  if (cache_mode) {
    cache_root <- .wss_cache_root(create = TRUE)
  } else {
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }
    cache_root <- destdir
  }

  destfiles <- character(0)
  if (length(urls) > 0) {
    destfiles <- .wss_cache_download_urls(
      urls = urls,
      cache_root = cache_root,
      cache_mode = cache_mode,
      force = force,
      quiet = quiet
    )
    if (length(destfiles) < length(urls)) {
      stop("Failed to download one or more SSURGO ZIP files.", call. = FALSE)
    }
  }

  zip_paths <- destfiles[file.exists(destfiles)]

  if (length(zip_paths) == 0) {
    stop("Could not find SSURGO ZIP files in the resolved download directory.", call. = FALSE)
  }

  if (extract) {
    if (is.null(exdir)) {
      if (cache_mode) {
        target_files <- if (length(zip_paths) > 0) zip_paths else destfiles
        unique_dirs <- unique(dirname(target_files[nzchar(target_files)]))
        if (length(unique_dirs) > 1) {
          stop("Cached ZIP files correspond to multiple fiscal years/directories. Explicit 'exdir' must be provided for extraction.", call. = FALSE)
        } else if (length(unique_dirs) == 1) {
          exdir <- unique_dirs[1]
        } else {
          exdir <- cache_root
        }
      } else {
        exdir <- destdir
      }
    }

    if (!quiet) {
      message("Extracting downloaded ZIP files...")
    }

    if (!dir.exists(exdir)) {
      dir.create(exdir, recursive = TRUE)
    }

    UNZIP.FUN <- function(i) {
      if (isTRUE(include_spatial) && isTRUE(include_tabular)) {
        lz <- NULL
      } else {
        lz <- utils::unzip(zip_paths[i], list = TRUE)$Name
        lz_rel <- gsub("\\\\", "/", lz)
        names(lz) <- lz_rel

        # need to pre-extract mstab data to map to real column names
        lz_meta <- lz_rel[grepl(
          "^(mstab|mdstattabs|MetadataTable|mstabcol|mdstattabcol|MetadataColumnLookup|msidxdet|mdstatidxdet|MetadataIndexDetail)$",
          tools::file_path_sans_ext(basename(lz_rel))
        )]
        if (length(lz_meta) > 0) {
          utils::unzip(zip_paths[i], files = unname(lz[lz_meta]), exdir = exdir)
        }

        # explicitly fetch internal function in our namespace to support parallel workers
        INV.FUN <- get(".inventory_ssurgo_files", envir = asNamespace("soilDB"))
        inv <- INV.FUN(lz_rel, exdir = exdir, include_spatial = include_spatial, include_tabular = include_tabular)

        lz <- unname(lz[unlist(c(inv$f.shp.sc, inv$f.txt.grp))])
      }
      uz <- utils::unzip(zip_paths[i], files = lz, exdir = exdir)
      if (length(uz) == 0) {
        message(paste('Invalid zipfile:', zip_paths[i]))
      } else {
        if (!quiet) {
          message("Extracted: ", zip_paths[i])
        }
      }
    }

    res <- do.call(LAPPLY.FUN, c(list(seq_along(zip_paths), UNZIP.FUN), LAPPLY.FUN.ARGS))

    if (remove_zip) {
      file.remove(zip_paths)
    }
  }

  invisible(unname(zip_paths))
}

.match_ssurgo_areasymbols <- function(files, areasymbols) {
  target_symbols <- unique(toupper(areasymbols))
  parts <- strsplit(gsub("\\\\", "/", files), "/", fixed = TRUE)
  file_symbols <- Map(function(path_parts) {
    basename_without_ext <- tools::file_path_sans_ext(path_parts[length(path_parts)])
    toupper(c(
      if (length(path_parts) > 1L) path_parts[-length(path_parts)] else character(),
      strsplit(basename_without_ext, "_", fixed = TRUE)[[1]]
    ))
  }, parts)
  available_symbols <- unique(unlist(file_symbols, use.names = FALSE))
  list(
    match = vapply(file_symbols, function(symbols) any(symbols %in% target_symbols), logical(1)),
    found = intersect(target_symbols, available_symbols)
  )
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
#' When `include_raster=TRUE`, the default coordinate reference systems (CRS) and 10m grid
#' alignments are designed for seamless interoperability with standard federal rasters including
#' NLCD (National Land Cover Database), CDL (Cropland Data Layer), USGS 3DEP elevation products,
#' and gNATSGO 10m grids.
#'
#' @param filename _character_. Output file name (e.g. `'db.sqlite'` or `'db.gpkg'`). Only used when
#'   `con` is not specified by the user.
#' @param exdir  _character_. Path containing input SSURGO spatial (.shp) and tabular
#'   (.txt) files, downloaded and extracted by `downloadSSURGO()` or similar. Default: `NULL` selects the most recent fiscal-year cache directory.
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
#' @param include_raster _logical_. Generate 10m MUKEY GeoTIFF rasters for each SSA?
#'   Default: `FALSE` does not create raster outputs. Set to `TRUE` to create georeferenced rasters cached in
#'   `soilDB_user_dir("cache", "WSS", ...)` and output a combined/copied raster matching `filename`.
#'   Requires `terra` package.
#' @param LAPPLY.FUN _function_. `lapply()`-like function to use for iteration during rasterization
#'   phase. Only used if `include_raster=TRUE`. This allows for the rasterization operations to be run
#'   in parallel instead of sequential, custom progress reporting, or similar.
#' @param LAPPLY.FUN.ARGS _list_. Optional list of additional arguments to pass to `LAPPLY.FUN`.
#' @param fiscal_year _character_. Optional fiscal year label for cache organization (e.g., `'FY2024'`,
#'   `'FY26'`). If `NULL`, auto-detect from sacatalog saverest date. Allows organizing rasters by official
#'   release year or custom labels. Only used if `include_raster=TRUE`.
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
#' @param na.strings _character_. Passed to `data.table::fread()`. Default: `c("", "NA")`
#' @param quote _character_. Passed to `data.table::fread()`. Default: `""`
#' @param template Optional `SpatRaster` template to align with. If `NULL` (default), creates a standard
#'   10m grid in the regional standard CRS (EPSG:5070 for CONUS, EPSG:3338 for AK, EPSG:6628 for HI,
#'   EPSG:32161 for PR/VI, EPSG:4326 for Pacific Islands) with bounds snapped according to `grid_alignment`.
#' @param touches _logical_. If `TRUE`, all cells touched by polygon boundaries are rasterized. Default
#'   `FALSE` rasterizes only cells whose centroids fall within polygons.
#' @param grid_alignment _character_. Grid alignment mode when `template = NULL`. One of `"legacy_gssurgo"` (or `"legacy"`, default, 5m offset matching official USDA-NRCS gSSURGO/gNATSGO/RSS products with `(coord - 5) %% 10 == 0`), `"target_aligned"` (0m offset standard GDAL -tap alignment with `coord %% 10 == 0`), or `"nlcd"` (canonical NLCD WGS84 Albers Equal Area CRS with origin anchor `(-2415585, 3314805)`).
#' @param quiet _logical_. Suppress messages and other output from database read/write operations?
#' @param areasymbols _character_. Optional character vector of soil survey area symbols (e.g. `c("CA067", "CA077")`) used to subset the folders/files within `exdir` to process. Default `NULL` processes all SSURGO exports found in `exdir`.
#' @param ... Additional arguments passed to `sf::write_sf()` for writing spatial layers.
#'
#' @return _character_. Vector of layer/table names in `filename`.
#' @seealso [downloadSSURGO()]
#' @importFrom stats sd
#' @importFrom utils flush.console head
#' @export
#' @examples
#' \dontrun{
#'  downloadSSURGO("areasymbol IN ('CA067', 'CA077', 'CA632')", destdir = "SSURGO_test")
#'  createSSURGO("test.gpkg", "SSURGO_test")
#' }
createSSURGO <- function(filename = NULL,
                         exdir = NULL,
                         conn = NULL,
                         pattern = NULL,
                         include_spatial = TRUE,
                         include_tabular = TRUE,
                         include_raster = FALSE,
                         LAPPLY.FUN = lapply,
                         LAPPLY.FUN.ARGS = NULL,
                         dissolve_field = NULL,
                         maxruledepth = 0,
                         overwrite = FALSE,
                         append = FALSE,
                         header = FALSE,
                         sep = "|",
                         na.strings = c("", "NA"),
                         quote = "",
                         fiscal_year = NULL,
                         template = NULL,
                         touches = FALSE,
                         grid_alignment = c("legacy_gssurgo", "legacy", "target_aligned", "nlcd"),
                         quiet = TRUE,
                         areasymbols = NULL,
                         ...) {

  grid_alignment <- .normalize_grid_alignment(grid_alignment)

  if ((missing(filename) || length(filename) == 0) && missing(conn)) {
    stop("`filename` should be a path to a .gpkg or .sqlite file to create or append to, or a DBIConnection should be provided via `conn`.")
  }

  # when exdir is not specified, take most recent FY cache dir
  if (is.null(exdir)) {

    wss_cache <- .wss_cache_root(create = FALSE)
    wss_cache_dirs <- if (dir.exists(wss_cache)) list.dirs(wss_cache, recursive = FALSE) else character(0)

    exdir <- sort(wss_cache_dirs[grepl("^FY\\d{2}$", basename(wss_cache_dirs))], decreasing = TRUE)

    if (length(exdir) >= 1) {
      exdir <- exdir[1]
    } else {
      stop(
        "No WSS cache FY directories found in cache root. Specify 'exdir' directly or run downloadSSURGO() first.",
        call. = FALSE
      )
    }
  }

  if (!quiet) {
    message("Scanning SSURGO extraction directory ", exdir, "...")
  }

  if (!dir.exists(exdir)) {
    stop(sprintf("SSURGO extraction directory (%s) appears to be empty, check that the path is correct.\n\nIf you want to download new data, run `downloadSSURGO(destdir=%s, ...)` first.",
                 exdir, shQuote(exdir)),
         call. = FALSE)
  }

  f_all <- list.files(exdir, recursive = TRUE, full.names = TRUE)
  if (length(f_all) == 0) {
    stop(sprintf("SSURGO extraction directory (%s) appears to be empty, check that the path is correct.\n\nIf you want to download new data, run `downloadSSURGO(destdir=%s, ...)` first.",
                 exdir, shQuote(exdir)),
         call. = FALSE)
  }

  file_extensions <- tolower(tools::file_ext(f_all))
  has_zips <- any(file_extensions == "zip")
  has_data <- any(file_extensions %in% c("shp", "txt"))
  if (has_zips && !has_data) {
    stop(
      sprintf(
        "SSURGO extraction directory (%s) contains ZIP files but no extracted SSURGO folders. Run 'downloadSSURGO(..., extract = TRUE)' or extract archives before calling createSSURGO().",
        shQuote(exdir)
      ),
      call. = FALSE
    )
  }
  
  # Validate terra package availability for rasterization
  if (isTRUE(include_raster)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("package 'terra' is required for include_raster=TRUE. ",
           "Install it with: install.packages('terra')",
           call. = FALSE)
    }
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
  f <- f_all
  fdx <- rep(TRUE, length(f))

  if (!is.null(areasymbols)) {
    areasymbols <- unique(toupper(areasymbols))
    ssa_matches <- .match_ssurgo_areasymbols(f, areasymbols)

    # Check if any requested areasymbols were completely missing
    missing_symbols <- setdiff(areasymbols, ssa_matches$found)
    if (length(missing_symbols) > 0) {
      warning("The following 'areasymbols' were not found in 'exdir': ",
              paste0(missing_symbols, collapse = ", "), call. = FALSE)
    }
    fdx <- fdx & ssa_matches$match
  }

  if (!is.null(pattern)) {
    fdx <- fdx & grepl(pattern, f)
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
  
  f_inventory <- f[fdx]

  # initialize timing records for this run (collected locally and attached to result)
  write_timings <- list()
  raster_paths <- list()
  zero_time <- Sys.time()
  
  inv <- .inventory_ssurgo_files(
    files = f_inventory,
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

  if (is.null(inv$shp.grp) && is.null(inv$mstabcn)) {
    stop(
      "Extraction directory `exdir` ",
      shQuote(exdir),
      " does not appear to contain SSURGO spatial data or tabular data",
      call. = FALSE
    )
  }

  if (!is.null(inv$shp.grp) && nrow(inv$shp.grp) >= 1 && ncol(inv$shp.grp) == 3 && include_spatial) {

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

  # Derive fiscal year from sacatalog if not provided by user
  derived_fiscal_year <- fiscal_year
  if (isTRUE(include_raster) && is.null(fiscal_year)) {
    # Try to extract fiscal year from sacatalog table in extracted SSURGO files
    derived_fiscal_year <- .derive_fiscal_year_from_sacatalog(inv$f.txt.grp, exdir, sep, quote, na.strings, header)

    # If derivation failed, error and inform user
    if (is.na(derived_fiscal_year)) {
      stop("Could not auto-detect fiscal_year from sacatalog. ",
           "Please specify fiscal_year explicitly (e.g., fiscal_year='FY2024')",
           call. = FALSE)
    }
  }

  # Rasterize mupolygon to MUKEY raster (parallelized by SSA if include_raster=TRUE)
  if (isTRUE(include_raster) && !is.null(inv$shp.grp) && nrow(inv$shp.grp) >= 1) {

    # Extract unique SSA directories from mupolygon shapefiles
    mu_idx <- which(inv$shp.grp[, 1] == "mu" & inv$shp.grp[, 2] == "a")

    if (length(mu_idx) > 0) {
      # Get mupolygon shapefiles (feature "mu", geometry "a") directly from inventory
      mu_files <- inv$f.shp[mu_idx]

      # Extract unique SSA directories and areasymbols
      ssa_dirs <- unique(dirname(mu_files))
      ssa_list <- lapply(ssa_dirs, function(d) {
        # Extract areasymbol from directory or filename
        mu_file <- mu_files[dirname(mu_files) == d][1]
        areasymbol <- gsub(".*soil[musfa]{2}_[apl]_([a-z]{2}\\d{3}|[a-z]{2})\\.shp.*",
                           "\\1",
                           mu_file,
                           ignore.case = TRUE)
        list(ssa_dir = d, areasymbol = areasymbol, fiscal_year = derived_fiscal_year)
      })

      # Define rasterization worker function for namespace isolation
      # Captures ssa_list and quiet from enclosing scope
      .RASTERIZE.FUN <- function(ssa_index) {
        if (ssa_index > length(ssa_list)) {
          return(list())
        }

        ssa_info <- ssa_list[[ssa_index]]
        ssa_dir <- ssa_info$ssa_dir
        areasymbol <- ssa_info$areasymbol
        fiscal_year_arg <- ssa_info$fiscal_year

        # Explicitly fetch internal functions from namespace for worker isolation
        .rasterize_fn <- get(".rasterize_ssurgo_ssa", envir = asNamespace("soilDB"))
        .write_raster_fn <- get(".write_raster_with_cache", envir = asNamespace("soilDB"))

        # Call rasterization helper
        mukey_raster <- .rasterize_fn(
          ssa_dir = ssa_dir,
          areasymbol = areasymbol,
          template = template,
          touches = touches,
          grid_alignment = grid_alignment,
          quiet = quiet
        )

        # Write raster to cache if rasterization succeeded
        if (!is.null(mukey_raster)) {
          raster_path <- .write_raster_fn(
            raster_obj = mukey_raster,
            areasymbol = areasymbol,
            fiscal_year = fiscal_year_arg,
            cache_root = NULL,
            quiet = quiet
          )

          if (!is.na(raster_path)) {
            return(list(list(
              areasymbol = areasymbol,
              path = raster_path
            )))
          }
        }
        list()
      }

      # Compute the unified vector extent before rasterization so final output
      # is cropped to the same snapped geometry bounds as the SSA templates.
      target_bbox <- .compute_ssa_vector_bbox(
        mu_files = mu_files,
        ssa_list = ssa_list,
        grid_alignment = grid_alignment,
        resolution_m = 10
      )

      # Execute rasterization in parallel or sequential mode
      raster_results <- do.call(LAPPLY.FUN, c(list(seq_along(ssa_list), .RASTERIZE.FUN), LAPPLY.FUN.ARGS))

      # Accumulate results into raster_paths (vectorized: filter empty lists and unlist one level)
      raster_paths <- c(raster_paths, unlist(Filter(length, raster_results), recursive = FALSE))

      # If filename or conn was provided, write/merge final output raster
      final_raster_path <- .create_final_raster_output(
        raster_paths = raster_paths,
        filename = filename,
        conn = conn,
        exdir = exdir,
        target_bbox = target_bbox,
        grid_alignment = grid_alignment,
        overwrite = overwrite,
        quiet = quiet
      )
      if (!is.null(final_raster_path) && !is.na(final_raster_path)) {
        attr(raster_paths, "output_raster") <- final_raster_path
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

  if (!is.null(inv$mstabcn) && include_tabular) {

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
        Integer = "integer", Float = "numeric"#, Boolean = "nonop" # see #482
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

      table_initialized <- FALSE

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

                if (!table_exists || !isTRUE(table_initialized)) {
                  append_arg <- FALSE
                  overwrite_arg <- table_exists || overwrite
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
                  table_initialized <- TRUE
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

  # attach raster cache paths as attribute for programmatic access
  if (length(raster_paths) > 0) {
    raster_paths_df <- do.call(rbind, lapply(raster_paths, function(r)
      data.frame(
        areasymbol = r$areasymbol,
        path = r$path,
        stringsAsFactors = FALSE
      )))
    if (!is.null(attr(raster_paths, "output_raster"))) {
      attr(raster_paths_df, "output_raster") <- attr(raster_paths, "output_raster")
    }
    attr(res, 'raster_paths') <- raster_paths_df
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
      numeric   = suppressWarnings(as.numeric(as.character(y[[col]])))
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
  .N <- NULL; elapsed <- NULL; total_elapsed <- NULL
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
  agg_dt <- dt[, list(
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

#' @noRd
#' @keywords internal
.normalize_grid_alignment <- function(grid_alignment) {
  grid_alignment <- match.arg(
    grid_alignment,
    c("legacy_gssurgo", "legacy", "target_aligned", "nlcd")
  )
  if (grid_alignment == "legacy") {
    grid_alignment <- "legacy_gssurgo"
  }
  grid_alignment
}
#' Resolve Standard CRS for Soil Survey Area
#'
#' @param areasymbol Soil survey area symbol or state code (e.g. "CA067", "AK650", "HI")
#' @param grid_alignment Grid alignment mode (e.g. "legacy_gssurgo", "nlcd")
#' @return Character CRS identifier (e.g. "EPSG:5070", "EPSG:3338", or PROJ string)
#' @noRd
#' @keywords internal
.resolve_ssa_crs <- function(areasymbol = NULL, grid_alignment = "legacy_gssurgo") {
  if (identical(grid_alignment, "nlcd")) {
    return("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  }

  if (!is.null(areasymbol) && length(areasymbol) > 0 && nchar(areasymbol[1]) >= 2) {
    prefix <- toupper(substr(areasymbol[1], 1, 2))
    if (prefix == "AK") {
      return("EPSG:3338")
    } else if (prefix == "HI") {
      return("EPSG:6628")
    } else if (prefix %in% c("PR", "VI")) {
      return("EPSG:32161")
    } else if (prefix %in% c("AS", "GU", "MP", "PW", "FM", "MH")) {
      return("EPSG:4326")
    }
  }
  "EPSG:5070"
}

#' @noRd
#' @keywords internal
.compute_ssa_vector_bbox <- function(mu_files, ssa_list,
                                     grid_alignment = c("legacy_gssurgo", "legacy", "target_aligned", "nlcd"),
                                     resolution_m = 10) {
  stopifnot(requireNamespace("sf", quietly = TRUE))

  grid_alignment <- .normalize_grid_alignment(grid_alignment)

  if (length(mu_files) == 0 || length(ssa_list) == 0) {
    return(NULL)
  }

  first_crs <- .resolve_ssa_crs(ssa_list[[1]]$areasymbol, grid_alignment = grid_alignment)
  bounds <- lapply(seq_along(mu_files), function(i) {
    mu <- sf::read_sf(mu_files[[i]], quiet = TRUE)
    if (nrow(mu) == 0) {
      return(NULL)
    }
    mu <- sf::st_transform(mu, first_crs)
    sf::st_bbox(mu)
  })
  bounds <- Filter(Negate(is.null), bounds)
  if (length(bounds) == 0) {
    return(NULL)
  }

  xmin <- min(vapply(bounds, function(x) x[["xmin"]], numeric(1)))
  xmax <- max(vapply(bounds, function(x) x[["xmax"]], numeric(1)))
  ymin <- min(vapply(bounds, function(x) x[["ymin"]], numeric(1)))
  ymax <- max(vapply(bounds, function(x) x[["ymax"]], numeric(1)))

  origin <- if (grid_alignment == "nlcd") {
    c(-2415585, 3314805)
  } else if (grid_alignment == "legacy_gssurgo") {
    c(resolution_m / 2, resolution_m / 2)
  } else {
    c(0, 0)
  }

  .snap_extent_to_grid(
    xmin, xmax, ymin, ymax,
    origin = origin,
    resolution = c(resolution_m, resolution_m)
  )
}

#' @noRd
#' @keywords internal
.snap_extent_to_grid <- function(xmin, xmax, ymin, ymax,
                                  origin = c(0, 0),
                                  resolution = c(1, 1)) {
  c(
    xmin = as.numeric(origin[1] + floor((xmin - origin[1]) / resolution[1]) * resolution[1]),
    xmax = as.numeric(origin[1] + ceiling((xmax - origin[1]) / resolution[1]) * resolution[1]),
    ymin = as.numeric(origin[2] + floor((ymin - origin[2]) / resolution[2]) * resolution[2]),
    ymax = as.numeric(origin[2] + ceiling((ymax - origin[2]) / resolution[2]) * resolution[2])
  )
}

#' Create SpatRaster Template for Soil Survey Area
#'
#' @param mu_shp sf object containing map unit polygons
#' @param template Optional SpatRaster template to align with
#' @param areasymbol Soil survey area symbol (e.g. "CA067")
#' @param resolution_m Resolution in meters (default: 10)
#' @param buffer_m Buffer in meters added around bbox before snapping (default: 0)
#' @param grid_alignment Grid alignment mode: `"legacy_gssurgo"` (or `"legacy"`, default), `"target_aligned"`, or `"nlcd"`
#' @return SpatRaster template covering the SSA extent snapped to grid
#' @noRd
#' @keywords internal
.create_ssa_raster_template <- function(mu_shp, template = NULL, areasymbol = NULL,
                                        resolution_m = 10, buffer_m = 0,
                                        grid_alignment = c("legacy_gssurgo", "legacy", "target_aligned", "nlcd")) {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("terra", quietly = TRUE))

  grid_alignment <- .normalize_grid_alignment(grid_alignment)

  if (inherits(template, "SpatRaster")) {
    target_crs <- terra::crs(template)
    mu_proj <- sf::st_transform(mu_shp, target_crs)
    extent_bbox <- sf::st_bbox(mu_proj)

    xmin <- extent_bbox[["xmin"]] - buffer_m
    xmax <- extent_bbox[["xmax"]] + buffer_m
    ymin <- extent_bbox[["ymin"]] - buffer_m
    ymax <- extent_bbox[["ymax"]] + buffer_m

    t_res <- terra::res(template)
    t_origin <- terra::origin(template)

    snapped <- .snap_extent_to_grid(xmin, xmax, ymin, ymax, origin = t_origin, resolution = t_res)
    xmin_snap <- snapped[["xmin"]]
    xmax_snap <- snapped[["xmax"]]
    ymin_snap <- snapped[["ymin"]]
    ymax_snap <- snapped[["ymax"]]

    terra::rast(
      xmin = xmin_snap,
      xmax = xmax_snap,
      ymin = ymin_snap,
      ymax = ymax_snap,
      resolution = t_res,
      crs = target_crs
    )
  } else {
    target_crs <- .resolve_ssa_crs(areasymbol, grid_alignment = grid_alignment)
    mu_proj <- sf::st_transform(mu_shp, target_crs)
    extent_bbox <- sf::st_bbox(mu_proj)

    xmin <- extent_bbox[["xmin"]] - buffer_m
    xmax <- extent_bbox[["xmax"]] + buffer_m
    ymin <- extent_bbox[["ymin"]] - buffer_m
    ymax <- extent_bbox[["ymax"]] + buffer_m

    origin <- if (grid_alignment == "nlcd") {
      c(-2415585, 3314805)
    } else if (grid_alignment == "legacy_gssurgo") {
      c(resolution_m / 2, resolution_m / 2)
    } else {
      c(0.0, 0.0)
    }

    snapped <- .snap_extent_to_grid(xmin, xmax, ymin, ymax, origin = origin, resolution = c(resolution_m, resolution_m))
    xmin_snap <- snapped[["xmin"]]
    xmax_snap <- snapped[["xmax"]]
    ymin_snap <- snapped[["ymin"]]
    ymax_snap <- snapped[["ymax"]]

    terra::rast(
      xmin = xmin_snap,
      xmax = xmax_snap,
      ymin = ymin_snap,
      ymax = ymax_snap,
      resolution = resolution_m,
      crs = target_crs
    )
  }
}


#' Rasterize SSURGO MUKEY to Standard Grid
#'
#' @description `.rasterize_ssurgo_ssa()`: Generate in-memory SpatRaster from MUKEY polygons
#'   for a single soil survey area (SSA).
#'
#' @details
#' Workflow:
#' 1. Extract MUKEY from SSURGO mapunit polygon shapefile from SSA directory
#' 2. Create standard or custom grid template via `.create_ssa_raster_template()`
#' 3. Project polygon features to template target CRS and convert MUKEY to numeric
#' 4. Rasterize polygon features to grid using MUKEY as cell values
#' 5. Return in-memory SpatRaster object for caller to handle writing
#'
#' @param ssa_dir _character_. Path to SSA extraction directory
#'   containing SSURGO spatial (.shp) files.
#' @param areasymbol _character_. Soil survey area symbol (e.g., "CA067") for logging.
#' @param template Optional SpatRaster template to align with.
#' @param touches _logical_. If TRUE, all cells touched by polygons are rasterized. Default FALSE.
#' @param grid_alignment Grid alignment mode: `"legacy_gssurgo"` (or `"legacy"`, default), `"target_aligned"`, or `"nlcd"`.
#' @param quiet _logical_. Suppress rasterization messages if TRUE.
#'
#' @return SpatRaster object (in-memory), or NULL if rasterization fails or mupolygon not found.
#' @noRd
#' @keywords internal
.rasterize_ssurgo_ssa <- function(ssa_dir, areasymbol, template = NULL, touches = FALSE,
                                  grid_alignment = c("legacy_gssurgo", "legacy", "target_aligned", "nlcd"),
                                  quiet = FALSE) {

  grid_alignment <- .normalize_grid_alignment(grid_alignment)

  # Locate mupolygon shapefile in ssa_dir
  mu_shp_pattern <- list.files(ssa_dir, pattern = "^soilmu_a_.*\\.shp$", full.names = TRUE)

  if (length(mu_shp_pattern) == 0) {
    if (!isTRUE(quiet)) {
      warning("No mupolygon shapefile found for ", areasymbol, "; skipping rasterization",
              call. = FALSE)
    }
    return(NULL)
  }

  mu_shp_file <- mu_shp_pattern[1]

  tryCatch({
    # Read mupolygon shapefile
    mu_shp <- sf::read_sf(mu_shp_file)

    # Validate shapefile has data
    if (nrow(mu_shp) == 0) {
      if (!isTRUE(quiet)) {
        warning("Mupolygon shapefile for ", areasymbol, " has no features; skipping rasterization",
                call. = FALSE)
      }
      return(NULL)
    }

    # Lowercase column names for consistency
    colnames(mu_shp) <- tolower(colnames(mu_shp))

    # Check for MUKEY field
    if (!("mukey" %in% colnames(mu_shp))) {
      if (!isTRUE(quiet)) {
        warning("MUKEY field not found in mupolygon shapefile for ", areasymbol,
                "; skipping rasterization", call. = FALSE)
      }
      return(NULL)
    }

    # Create SpatRaster template snapped to grid
    rast_template <- .create_ssa_raster_template(
      mu_shp,
      template = template,
      areasymbol = areasymbol,
      grid_alignment = grid_alignment
    )

    # Project polygons into template target CRS
    target_crs <- terra::crs(rast_template)
    mu_proj <- sf::st_transform(mu_shp, target_crs)

    # Convert mukey values to numeric for clean integer cell values
    mu_proj$mukey <- as.numeric(mu_proj$mukey)

    # Convert sf object to SpatVector for terra::rasterize
    mu_vec <- terra::vect(mu_proj)

    # Rasterize MUKEY field to template grid
    mukey_raster <- terra::rasterize(
      mu_vec,
      rast_template,
      field = "mukey",
      touches = touches
    )

    if (!isTRUE(quiet)) {
      message("Rasterizing MUKEY for ", areasymbol, "...")
    }

    return(mukey_raster)

  }, error = function(e) {
    if (!isTRUE(quiet)) {
      warning("Rasterization failed for ", areasymbol, ": ", conditionMessage(e),
              call. = FALSE)
    }
    return(NULL)
  })
}

#' Derive Fiscal Year from SSURGO Metadata
#'
#' @description Extract saverest date from sacatalog table in extracted SSURGO files
#'   and derive fiscal year (Oct-Sep) as "FY{YY}" format.
#'
#' @param f.txt.grp _list_. Named list of tabular text file groups from inventory.
#' @param exdir _character_. Extraction directory containing SSURGO files.
#' @param sep _character_. Field separator for reading delimited files.
#' @param quote _character_. Quote character for reading delimited files.
#' @param na.strings _character_. NA string representation.
#' @param header _logical_. Whether files have headers.
#'
#' @return _character_. Fiscal year in "FY{YY}" format (e.g., "FY24"), or NA if not found.
#' @noRd
#' @keywords internal
.derive_fiscal_year_from_sacatalog <- function(f.txt.grp, exdir, sep = "|", quote = "", na.strings = c("", "NA"), header = FALSE) {

  # Try to find sacatalog table file
  sacatalog_file <- NULL
  if (!is.null(f.txt.grp[["sacatalog"]])) {
    sacatalog_file <- f.txt.grp[["sacatalog"]][1]
  }

  if (is.null(sacatalog_file) || !file.exists(sacatalog_file)) {
    return(NA_character_)
  }

  tryCatch({
    # Read sacatalog table
    sacatalog <- suppressWarnings(data.table::fread(
      sacatalog_file,
      sep = sep,
      header = header,
      na.strings = na.strings,
      showProgress = FALSE,
      data.table = FALSE
    ))

    if (is.null(sacatalog) || nrow(sacatalog) == 0) {
      return(NA_character_)
    }

    # Look for saverest column (typically in column 3 or 4)
    saverest_col <- NULL
    for (col_name in c("saverest", "V3", "V4")) {
      if (col_name %in% colnames(sacatalog)) {
        saverest_col <- col_name
        break
      }
    }

    if (is.null(saverest_col)) {
      return(NA_character_)
    }

    # Parse first saverest date
    saverest_str <- as.character(sacatalog[[saverest_col]][1])
    if (is.na(saverest_str) || nzchar(saverest_str) == 0) {
      return(NA_character_)
    }

    # Parse date - try both format with timestamp and date-only
    saverest <- as.Date(saverest_str, format = "%m/%d/%Y %H:%M:%S")
    if (is.na(saverest)) {
      saverest <- as.Date(saverest_str)
    }

    if (is.na(saverest)) {
      return(NA_character_)
    }

    # Derive fiscal year using existing WSS function
    fiscal_year <- .wss_fiscal_year(saverest)
    return(fiscal_year)

  }, error = function(e) {
    return(NA_character_)
  })
}

#' Write SpatRaster to Cache with terra Fallback
#'
#' @description `.write_raster_with_cache()`: Write in-memory SpatRaster to disk cache
#'   using soilDB user directory pattern. Falls back to terra::tmpdir() if primary
#'   cache write fails.
#'
#' @param raster_obj _SpatRaster_. In-memory raster object to persist.
#' @param areasymbol _character_. Soil survey area symbol (e.g., "CA067") for filename.
#' @param cache_root _character_. Cache root directory. If NULL, defaults to
#'   soilDB_user_dir("cache", "raster", create=TRUE).
#' @param fiscal_year _character_. Optional fiscal year label for cache path organization
#'   (e.g., `"FY2024"`, `"FY26"`). If provided, raster is written to
#'   `soilDB_user_dir("cache", "WSS", {fiscal_year}, "raster")`. If `NULL`, uses legacy cache path.
#' @param quiet _logical_. Suppress write messages if TRUE.
#'
#' @return _character_. Normalized path to written GeoTIFF, or NA_character_ if write failed.
#' @noRd
#' @keywords internal
.write_raster_with_cache <- function(raster_obj, areasymbol, fiscal_year = NULL, cache_root = NULL, quiet = FALSE) {

  # Determine cache root directory
  if (is.null(cache_root)) {
    if (!is.null(fiscal_year)) {
      # Nested cache structure: soilDB_user_dir("cache", "WSS", fiscal_year, "raster")
      wss_cache_root <- .wss_cache_root(create = TRUE)
      cache_root <- file.path(wss_cache_root, fiscal_year, "raster")
    } else {
      # Legacy flat cache structure: soilDB_user_dir("cache", "raster")
      cache_root <- soilDB_user_dir("cache", "raster", create = TRUE)
    }
  }
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

  # Construct filename
  filename <- paste0("MUKEY_10m_", toupper(areasymbol), ".tif")
  cache_path <- file.path(cache_root, filename)

  tryCatch({
    # Ensure raster values are compatible with unsigned integer representation
    if (inherits(raster_obj, "SpatRaster")) {
      raster_obj <- terra::as.int(raster_obj)
    }

    # Write raster to primary cache location
    terra::writeRaster(
      raster_obj,
      cache_path,
      overwrite = TRUE,
      datatype = "INT4U",
      NAflag = 0,
      filetype = "COG",
      gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")
    )

    if (!isTRUE(quiet)) {
      message("Wrote raster to cache: ", cache_path)
    }

    # Return normalized path
    normalizePath(cache_path, winslash = .Platform$file.sep, mustWork = FALSE)

  }, error = function(e) {
    # Fallback: write to temporary directory
    tmpdir <- terra::tmpdir()
    tmppath <- file.path(tmpdir, filename)

    tryCatch({
      terra::writeRaster(
        raster_obj,
        tmppath,
        overwrite = TRUE,
        datatype = "INT4U",
        NAflag = 0,
        filetype = "COG",
        gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")
      )

      warning("Cache directory unavailable; raster written to temporary directory: ",
              tmppath, call. = FALSE)

      normalizePath(tmppath, winslash = .Platform$file.sep, mustWork = FALSE)

    }, error = function(e2) {
      warning("Failed to write raster for ", areasymbol, ": ", conditionMessage(e2),
              call. = FALSE)
      return(NA_character_)
    })
  })
}

#' Create Final Output Raster from SSA Rasters
#'
#' Copies single SSA raster or merges/mosaics multiple SSA rasters into a final output GeoTIFF.
#' If vector bounding box / shapefile extents are available, trims/crops to the exact snapped vector extent.
#'
#' @param raster_paths List of list(areasymbol, path) objects.
#' @param filename Database filename (e.g. "path/to/test.gpkg").
#' @param conn DBIConnection (used to infer dbname if filename is NULL).
#' @param exdir Directory containing SSURGO files.
#' @param target_bbox Optional numeric bounding box or SpatExtent to crop the final output to.
#' @param grid_alignment Grid alignment mode: `"legacy_gssurgo"` (default), `"target_aligned"`, or `"nlcd"`.
#' @param resolution_m Target raster resolution in meters (default: 10).
#' @param overwrite Overwrite existing output raster?
#' @param quiet Suppress messages if TRUE.
#'
#' @return Normalized path to final output raster, or NULL.
#' @noRd
#' @keywords internal
.create_final_raster_output <- function(raster_paths,
                                       filename = NULL,
                                       conn = NULL,
                                       exdir = NULL,
                                       target_bbox = NULL,
                                       grid_alignment = c("legacy_gssurgo", "legacy", "target_aligned", "nlcd"),
                                       resolution_m = 10,
                                       overwrite = FALSE,
                                       quiet = FALSE) {
  if (is.null(raster_paths) || length(raster_paths) == 0) {
    return(NULL)
  }

  grid_alignment <- .normalize_grid_alignment(grid_alignment)

  paths <- unlist(lapply(raster_paths, function(x) x$path), use.names = FALSE)
  paths <- paths[!is.na(paths) & file.exists(paths)]

  if (length(paths) == 0) {
    return(NULL)
  }

  # Determine target output raster directory and base name
  target_file <- NULL
  if (!is.null(filename) && is.character(filename) && length(filename) > 0 && nzchar(filename[1])) {
    target_file <- sub("\\.[^.]+$", ".tif", filename[1])
    if (!grepl("\\.tif$", target_file, ignore.case = TRUE)) {
      target_file <- paste0(target_file, ".tif")
    }
  } else if (!is.null(conn) && inherits(conn, "DBIConnection")) {
    dbname <- tryCatch(conn@dbname, error = function(e) NULL)
    if (!is.null(dbname) && is.character(dbname) && length(dbname) > 0 && nzchar(dbname[1])) {
      target_file <- sub("\\.[^.]+$", ".tif", dbname[1])
      if (!grepl("\\.tif$", target_file, ignore.case = TRUE)) {
        target_file <- paste0(target_file, ".tif")
      }
    }
  }

  if (is.null(target_file) && !is.null(exdir)) {
    target_file <- file.path(exdir, "mukey.tif")
  }

  if (is.null(target_file)) {
    return(NULL)
  }

  dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(target_file) && !isTRUE(overwrite)) {
    if (!isTRUE(quiet)) {
      message("Output raster already exists; skipping overwrite: ", target_file)
    }
    return(normalizePath(target_file, winslash = .Platform$file.sep, mustWork = FALSE))
  }

  tryCatch({
    # Determine target SpatRaster (single source or mosaic)
    if (length(paths) == 1) {
      out_r <- terra::rast(paths[1])
    } else {
      sprc <- terra::sprc(paths)
      out_r <- terra::mosaic(sprc)
    }

    # If target_bbox is provided, crop output raster to exact snapped bounds
    if (!is.null(target_bbox)) {
      if (inherits(target_bbox, c("bbox", "numeric", "integer")) && length(target_bbox) >= 4) {
        tb_names <- names(target_bbox)
        xmin_val <- if ("xmin" %in% tb_names) target_bbox[["xmin"]] else target_bbox[1]
        ymin_val <- if ("ymin" %in% tb_names) target_bbox[["ymin"]] else target_bbox[2]
        xmax_val <- if ("xmax" %in% tb_names) target_bbox[["xmax"]] else target_bbox[3]
        ymax_val <- if ("ymax" %in% tb_names) target_bbox[["ymax"]] else target_bbox[4]

        origin <- if (grid_alignment == "nlcd") {
          c(-2415585, 3314805)
        } else if (grid_alignment == "legacy_gssurgo") {
          c(resolution_m / 2, resolution_m / 2)
        } else {
          c(0.0, 0.0)
        }

        snapped <- .snap_extent_to_grid(xmin_val, xmax_val, ymin_val, ymax_val,
                                        origin = origin,
                                        resolution = c(resolution_m, resolution_m))
        crop_ext <- terra::ext(snapped[["xmin"]], snapped[["xmax"]], snapped[["ymin"]], snapped[["ymax"]])
        out_r <- terra::crop(out_r, crop_ext)
      } else if (inherits(target_bbox, "SpatExtent")) {
        out_r <- terra::crop(out_r, target_bbox)
      }
    }

    # Write final raster output as COG GeoTIFF
    terra::writeRaster(
      out_r,
      target_file,
      overwrite = TRUE,
      datatype = "INT4U",
      NAflag = 0,
      filetype = "COG",
      gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")
    )

    if (!isTRUE(quiet)) {
      message("Created final raster output: ", target_file)
    }

    normalizePath(target_file, winslash = .Platform$file.sep, mustWork = FALSE)
  }, error = function(e) {
    if (!isTRUE(quiet)) {
      warning("Failed to create final output raster: ", conditionMessage(e), call. = FALSE)
    }
    NULL
  })
}
