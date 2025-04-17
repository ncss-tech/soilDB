#' Fetch Harmonized World Soil Database Data
#'
#' Creates a local cache of FAO Harmonized World Soil Database (HWSD)
#' information. Source raster map in ESRI Grid format (.bil) is converted to GeoTIFF.
#' The source tabular database in Microsoft Access (.mdb) format is converted to SQLite.
#' 
#' @param x Extent of interest that is passed to `terra::crop()` `y` argument. A
#'   SpatRaster, SpatVector, SpatExtent, or any other object that has a
#'   SpatExtent. Default `NULL` returns the full dataset.
#' @param hwsd_url URL for downloading HWSD dataset. Default
#'   `"https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/"`
#' @param force Force rebuilding of cache. Default: `FALSE`
#' @source Food and Agriculture Organization of the United Nations (FAO), Soils
#'   Portal, Harmonized World Soil Database (HWSD) v2.0
#'   \url{https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/}
#' @return A SpatRaster object with mapunit-level aggregate information stored
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
#'   terra::plot(res)
#'   
#'   # convert categories containing numeric data to numeric values
#'   res2 <- terra::catalyze(res)
#'   
#'   terra::plot(res2$AWC, main = "Available Water Capacity, mm")
#'   terra::lines(x, col = "white")
#' }
fetchHWSD <- function(x = NULL, 
                      hwsd_url = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/", 
                      force = FALSE) {
  
  # handling for future versions (older versions not available)
  HWSD_VERSION <- 2L
  
  data_dir <- file.path(tools::R_user_dir("soilDB", "data"), paste0("HWSD", HWSD_VERSION))
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  
  tiff_path <- file.path(data_dir, sprintf("HWSD%s.tif", HWSD_VERSION))
  if (isTRUE(force) || !file.exists(tiff_path)) {
    bil_url <- paste0(hwsd_url,
                      sprintf("HWSD%s_RASTER.zip/HWSD%s.bil", HWSD_VERSION))
    r <- terra::writeRaster(terra::rast(paste0("/vsizip/vsicurl/", bil_url)), 
                            tiff_path, 
                            datatype = "INT4U")
  } else {
    r <- terra::rast(tiff_path)
  }
  
  sqlite_path <- file.path(data_dir, "HWSD2.sqlite")
  mdb_path <- file.path(data_dir, "HWSD2.mdb")
  mdb_zip_path <- file.path(data_dir, "HWSD2_DB.zip")
  
  if (isTRUE(force) || !file.exists(mdb_path)){
    mdb_url <- paste0(hwsd_url, "HWSD2_DB.zip")
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
  
  hwsd2_smu <- soilDB::SDA_query("SELECT * FROM HWSD2_SMU", sqlite_path)
  levels(r2) <- hwsd2_smu[-1]
  r2
}

HWSD_path <- function(HWSD_VERSION = 2L) {
  # TODO abstract this logic; see https://github.com/ncss-tech/soilDB/issues/377
  data_dir <- file.path(tools::R_user_dir("soilDB", "data"), paste0("HWSD", HWSD_VERSION))
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  file.path(data_dir, "HWSD%s.sqlite")
}
