#' Fetch Harmonized World Soil Database Data
#'
#' Creates a local cache of FAO Harmonized World Soil Database (HWSD)
#' information. Source raster map in ESRI Grid format (.bil) is converted to GeoTIFF.
#' The source tabular database in Microsoft Access (.mdb) format is converted to SQLite.
#' 
#' @param x A _SpatRaster_, _SpatVector_, _SpatExtent_, or any other object that has
#'   a _SpatExtent_. Default `NULL` returns the full dataset. Extent of interest
#'   that is passed to `terra::crop()` `y` argument.
#' @param hwsd_url _character_. URL for downloading HWSD dataset. Default
#'   `"https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/"`
#' @param hwsd_version _integer_. Default `2L`. 
#' @param force Force rebuilding of cache. Default: `FALSE`
#' @source Food and Agriculture Organization of the United Nations (FAO), Soils
#'   Portal, Harmonized World Soil Database (HWSD) v2.0
#'   \url{https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/}
#' @return A _SpatRaster_ object with mapunit-level aggregate information stored
#'   as categories.
#' @export
#' @importFrom utils unzip
#' @examplesIf requireNamespace("terra", quietly = TRUE)
#' \dontrun{
#'   x <- terra::vect(system.file("ex", "lux.shp", package = "terra"))
#'   res <- fetchHWSD(x)
#'   
#'   # categorical data (WRB class)
#'   terra::activeCat(res) <- "WRB4"
#'   
#'   # view WRB4 map
#'   terra::plot(res)
#'   terra::lines(x, col = "white")
#'   
#'   # convert categories containing numeric data to numeric values
#'   res2 <- terra::catalyze(res)
#'   
#'   # view AWC map
#'   terra::plot(res2$AWC, main = "Available Water Capacity, mm")
#'   terra::lines(x, col = "white")
#'   
#'   # access tabular data from cached SQLite database
#'   SDA_query("SELECT * FROM HWSD2_SMU LIMIT 1", dsn = get_HWSD_path())
#' }
fetchHWSD <- function(x = NULL, 
                      hwsd_url = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/", 
                      hwsd_version = 2L,
                      force = FALSE) {
  tiff_path <- get_HWSD_path("raster", hwsd_version)
  data_dir <- dirname(tiff_path)
  if (isTRUE(force) || !file.exists(tiff_path)) {
    bil_url <- paste0(hwsd_url,
                      sprintf("HWSD%s_RASTER.zip/HWSD%s.bil", hwsd_version))
    r <- terra::writeRaster(terra::rast(paste0("/vsizip/vsicurl/", bil_url)), 
                            tiff_path, 
                            datatype = "INT4U")
  } else {
    r <- terra::rast(tiff_path)
  }
  
  sqlite_path <- file.path(data_dir, sprintf("HWSD%s.sqlite", hwsd_version))
  mdb_path <- file.path(data_dir, sprintf("HWSD%s.mdb", hwsd_version))
  mdb_zip_path <- file.path(data_dir, sprintf("HWSD%s_DB.zip", hwsd_version))
  
  if (isTRUE(force) || !file.exists(mdb_path)){
    mdb_url <- paste0(hwsd_url, sprintf("HWSD%s_DB.zip", hwsd_version))
    curl::curl_download(
      mdb_url, 
      destfile = mdb_zip_path, 
      quiet = TRUE,
      handle = .soilDB_curl_handle()
    )
    utils::unzip(
      mdb_zip_path,
      exdir = data_dir,
      overwrite = force,
      junkpaths = TRUE
    )
  }
  
  if (isTRUE(force) || !file.exists(sqlite_path)) {
    .mdb2sqlite(mdb_path, quiet = TRUE)
  }
  
  if (!is.null(x)){
    r2 <- terra::crop(r, x)
  } else {
    r2 <- r
  }
  
  # add soil mapunit information to attribute table
  hwsd_smu <- soilDB::SDA_query(sprintf("SELECT * FROM HWSD%s_SMU", hwsd_version), sqlite_path)
  if (!inherits(hwsd_smu, 'try-error')) {
    levels(r2) <- hwsd_smu[-1]
  }
  r2
}

#' @param what _character_. One of "sqlite", "raster", or "path"
#' @export
#' @rdname fetchHWSD
get_HWSD_path <- function(what = c("sqlite", "mdb", "raster", "path"), hwsd_version = 2L) {
  if (missing(what)) {
    what <- what[1]
  }
  if (is.null(what) || is.na(what) || isFALSE(nzchar(what))) {
    what <- "path"
  }
  what <- match.arg(what, c("sqlite", "mdb", "raster", "path"))
  extension <- switch(what,
                   "sqlite" = "sqlite",
                   "mdb" = "mdb",
                   "raster" = "tif",
                   NULL)
  base_dir <- soilDB_user_dir("data", paste0("HWSD", hwsd_version))
  if (is.null(extension)) {
    return(base_dir)
  }
  file.path(base_dir, sprintf("HWSD%s.%s", hwsd_version, extension))
}
