
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



# get pre-cached series extent GeoJSON or GeoTiff from SoilWeb server
seriesExtent <- function(s, type = c('vector', 'raster'), timeout=60) {
  if(!requireNamespace('rgdal', quietly=TRUE) | !requireNamespace('raster', quietly=TRUE))
    stop('please install the `rgdal` and `raster` packages', call.=FALSE)
  
  type <- match.arg(type)
  
  # encode series name
  s <- gsub(pattern=' ', replacement='_', x=tolower(s))
  
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
  
  # download GeoJSON file
  download.file(url=u, destfile=tf, extra=c(timeout=timeout), quiet=TRUE)
  
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
  
  # download GeoJSON file
  download.file(url=u, destfile=tf, extra=c(timeout=timeout), quiet=TRUE)
  
  # load into sp object and clean-up
  x <- raster::raster(tf, verbose=FALSE)
  x <- readAll(x)
  unlink(tf)
  
  # CONUS AEA
  return(x)
}

