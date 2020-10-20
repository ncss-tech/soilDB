
## this isn't going to work anymore, unless you have a GM API key
# # get the series extent from SEE pre-cached GeoJSON data and plot on Google Maps
# seriesExtentAsGmap <- function(s, timeout=60, exp=1.25) {
#   .Deprecated(msg = 'Google API no longer accepting requests without an API key. Consider using mapview::mapview().')
#   
#   return(FALSE)
#   
#   if(!requireNamespace('dismo', quietly=TRUE)  & !requireNamespace('rgdal', quietly=TRUE))
#     stop('please install the `rgdal` and `dismo` packages', call.=FALSE)
#   
# 	# load series extent data in WGS84 GCS
# 	x <- seriesExtent(s, timeout)
# 	
# 	# make extent object around sites, in geographic coordinates
# 	e <- raster::extent(sp::spTransform(x, sp::CRS('+proj=longlat')))
# 	
# 	# grab ref. to google maps
# 	g <- dismo::gmap(e, exp=exp)
# 	
# 	# convert our points to Mercatur projection
# 	x.M <- sp::spTransform(x, sp::CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs'))
# 	
# 	# plot google map, and our point of interest
# 	# note: have to use special plot methods
# 	# http://stackoverflow.com/questions/38818682/self-authored-package-load-plot-method-for-spatialpolygonsdataframe
# 	raster::plot(g, interpolate=TRUE)
# 	sp::plot(x.M, col=rgb(1, 0, 0, alpha=0.5), add=TRUE)
# }





#' @title Retrieve Soil Series Extent Maps from SoilWeb
#' 
#' @description This function downloads a generalized representations of a soil series extent from SoilWeb, derived from the current SSURGO snapshot. Data can be returned as vector outlines (\code{SpatialPolygonsDataFrame} object) or gridded representation of area proportion falling within 800m cells (\code{raster} object). Gridded series extent data are only available in CONUS. Vector representations are returned with a GCS/WGS84 coordinate reference system and raster representations are returned with an Albers Equal Area / NAD83 coordinate reference system.
#' 
#' @param s a soil series name, case-insensitive
#' 
#' @param type series extent representation, \code{vector} results in a \code{SpatialPolygonsDataFrame} object and \code{raster} results in a \code{raster} object
#' 
#' @param timeout time that we are willing to wait for a response, in seconds
#' 
#' @references \url{https://casoilresource.lawr.ucdavis.edu/see/}
#' 
#' @author D.E. Beaudette
#' 
#' @note This function requires the \code{rgdal} package. Warning messages about the proj4 CRS specification may be printed depending on your version of \code{rgdal}. This should be resolved soon.
#' 
#' @examples
#'   
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'   
#'   # required packages
#'   library(sp)
#'   library(raster)
#'   library(rgdal)
#'   
#'   # specify a soil series name
#'   s <- 'magnor'
#'   
#'   # return as SpatialPolygonsDataFrame
#'   x <- seriesExtent(s, type = 'vector')
#'   # return as raster
#'   y <- seriesExtent(s, type = 'raster')
#'   
#'   # note that CRS are different
#'   proj4string(x)
#'   projection(y)
#'   
#'   # transform vector representation to CRS of raster
#'   x <- spTransform(x, CRS(projection(y)))
#'   
#'   # graphical comparison
#'   par(mar = c(1, 1 , 1, 3))
#'   plot(y, axes = FALSE)
#'   plot(x, add = TRUE)
#'   
#'   
#' }
#' }
#' 

seriesExtent <- function(s, type = c('vector', 'raster'), timeout = 60) {
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  type <- match.arg(type)
  
  # encode series name: spaces -> underscores
  s <- gsub(pattern=' ', replacement='_', x = tolower(s), fixed = TRUE)
  
  # select type of output
  res <- switch(
    type,
    vector = {.vector_extent(s, timeout = timeout)},
    raster = {.raster_extent(s, timeout = timeout)}
  )
  
  return(res)
}

.vector_extent <- function(s, timeout) {
  # base URL to cached data
  u <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/series-extent-cache/json/', s, '.json'))
  
  # init temp files
  tf <- tempfile(fileext='.json')
  
  # safely download GeoJSON file
  res <- tryCatch(
    suppressWarnings(
      download.file(url=u, destfile=tf, extra=c(timeout = timeout), quiet=TRUE)
    ),
    error = function(e) {e}
  )

  # trap errors
  if(inherits(res, 'error')){
    message('no data returned')
    return(NULL)
  }
    
  # load into sp object and clean-up
  x <- rgdal::readOGR(dsn=tf, verbose=FALSE)
  unlink(tf)
  
  # reset row names in attribute data to series name
  x <- spChFIDs(x, as.character(x$series))
  
  # GCS WGS84
  return(x)
}

.raster_extent <- function(s, timeout) {
  # base URL to cached data
  u <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/series-extent-cache/grid/', s, '.tif'))
  
  # init temp files
  tf <- tempfile(fileext='.tif')
  
  # safely download GeoTiff file
  # Mac / Linux: file automatically downloaded via binary transfer
  # Windows: must manually specify binary transfrer
  res <- tryCatch(
    suppressWarnings(
      download.file(url = u, destfile = tf, extra = c(timeout = timeout), quiet=TRUE, mode = 'wb')  
    ),
    error = function(e) {e}
  )
  
  # trap errors
  if(inherits(res, 'error')){
    message('no data returned')
    return(NULL)
  }
  
  # load raster object into memory
  x <- raster(tf, verbose=FALSE)
  x <- readAll(x)
  # transfer layer name
  names(x) <- gsub(pattern='_', replacement=' ', x = s, fixed = TRUE)
  
  # cleanup
  unlink(tf)
  
  # CONUS AEA
  return(x)
}

