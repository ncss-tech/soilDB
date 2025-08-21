
## TODO:
# * consider using terra vs. sf for vector handling
# * consider direct GDAL access vs. curl download:
#    x <- vect('https://soilmap2-1.lawr.ucdavis.edu/ecoclassid-extent-cache/json/R109XY031MO.json')

#' @title Retrieve Soil Series Extent Maps from SoilWeb
#' 
#' @description This function downloads a generalized representations of a soil series extent from SoilWeb, derived from the current SSURGO snapshot. Data can be returned as vector outlines (`sf` object) or gridded representation of area proportion falling within 800m cells (`SpatRaster` object). Gridded series extent data are only available in CONUS. Vector representations are returned with a GCS/WGS84 coordinate reference system and raster representations are returned with an Albers Equal Area / NAD83 coordinate reference system (`EPSG:5070`).
#' 
#' @param s a soil series name, case-insensitive
#' @param type series extent representation, `'vector'`: results in an `sf` object and `'raster'` results in a `SpatRaster` object
#' @param timeout time that we are willing to wait for a response, in seconds
#' @param as_Spatial Return sp (`SpatialPolygonsDataFrame`) / raster (`RasterLayer`) classes? Default: `FALSE`.
#' @return An R spatial object, class depending on `type` and `as_Spatial` arguments
#' @references \url{https://casoilresource.lawr.ucdavis.edu/see/}
#' @author D.E. Beaudette
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE) && requireNamespace("terra") && requireNamespace("sf")
#' @export
#' @examples
#' \dontrun{
#'   
#'   # specify a soil series name
#'   s <- 'magnor'
#'   
#'   # return an sf object
#'   x <- seriesExtent(s, type = 'vector')
#'   
#'   # return a terra SpatRasters
#'   y <- seriesExtent(s, type = 'raster')
#'   
#'   library(terra)
#'   if (!is.null(x) && !is.null(y)) {
#'     x <- terra::vect(x)
#'     # note that CRS are different
#'     terra::crs(x)
#'     terra::crs(y)
#'   
#'     # transform vector representation to CRS of raster
#'     x <- terra::project(x, terra::crs(y))
#'   
#'     # graphical comparison
#'     par(mar = c(1, 1 , 1, 3))
#'     plot(y, axes = FALSE)
#'     plot(x, add = TRUE)
#'   }
#' }
seriesExtent <- function(s, type = c('vector', 'raster'), timeout = 60, 
                         as_Spatial = getOption('soilDB.return_Spatial', default = FALSE)) {
  
  # download timeout should be longer than default (13 seconds) 
  h <- .soilDB_curl_handle(timeout = timeout)
  
  # sanity check on type
  type <- match.arg(type)
  
  # encode series name: spaces -> underscores
  s <- gsub(pattern = ' ', replacement = '_', x = tolower(s), fixed = TRUE)
  
  # select type of output
  # ch: this is a shared curl handle with options set
  res <- switch(
    type,
    vector = {.vector_extent(s, ch = h, as_Spatial = as_Spatial)},
    raster = {.raster_extent(s, ch = h, as_Spatial = as_Spatial)}
  )
  
  return(res)
}

# 2022-08-15: converted from download.file() -> curl::curl_download() due to SSL errors
.vector_extent <- function(s, ch, as_Spatial) {
  
  if (!requireNamespace("sf")) 
    stop("package sf is required to return vector series extent grids", call. = FALSE)
  
  # base URL to cached data
  u <- URLencode(paste0('http://soilmap4-1.lawr.ucdavis.edu/series-extent-cache/json/', s, '.json'))
  
  res <- .soilDB_curl_get_JSON(u, gzip = FALSE, FUN = function(x) sf::st_read(x, quiet = TRUE), quiet = TRUE)
  
  # trapped errors return NULL
  if (is.null(res)) {
    message('no data returned for series: ', s)
    return(NULL)
  }
  
  # reset row names in attribute data to series name
  rownames(res) <- as.character(res$series)
  
  if (as_Spatial) {
    res <- sf::as_Spatial(res)
  }
  
  # GCS WGS84
  return(res)
}

# 2022-08-15: converted from download.file() -> curl::curl_download() due to SSL errors
.raster_extent <- function(s, ch, as_Spatial) {
  
  if (!requireNamespace("terra")) 
    stop("package terra is required to return raster series extent grids", call. = FALSE)
  
  # base URL to cached data
  u <- URLencode(paste0('http://soilmap4-1.lawr.ucdavis.edu/series-extent-cache/grid/', s, '.tif'))
  
  # init temp files
  tf <- tempfile(fileext = '.tif')
  
  # download GeoTiff file
  res <- try(curl::curl_download(url = u, destfile = tf, quiet = TRUE, handle = ch), silent = TRUE)
  
  # trap errors
  if (inherits(res, 'try-error')) {
    # message(res[1])
    message('no data returned')
    return(NULL)
  }
  
  # init SpatRaster
  x <- terra::rast(tf)
  
  # load all values into memory
  terra::values(x) <- terra::values(x)
  
  # remove tempfile 
  unlink(tf)
  
  # transfer layer name
  names(x) <- gsub(pattern = '_', replacement = ' ', x = s, fixed = TRUE)
  
  # make CRS explicit
  terra::crs(x) <- 'EPSG:5070'
  
  if (as_Spatial) {
    if (requireNamespace("raster", quietly = TRUE)) {
      x <- raster::raster(x) 
    } else {
      stop("Package `raster` is required to return raster data as a RasterLayer object with soilDB.return_Spatial=TRUE")
    }
  }
  
  return(x)
}

