# get the series extent from SEE pre-cached GeoJSON data and plot on Google Maps
seriesExtentAsGmap <- function(s, timeout=60, exp=1.25) {
  if(!requireNamespace('dismo', quietly=TRUE) & !requireNamespace('raster', quietly=TRUE) & !requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `raster`, `rgdal` and `dismo` packages', call.=FALSE)
  
	# load series extent data in WGS84 GCS
	x <- seriesExtent(s, timeout)
	
	# make extent object around sites, in geographic coordinates
	e <- raster::extent(spTransform(x, CRS('+proj=longlat')))
	
	# grab ref. to google maps
	g <- dismo::gmap(e, exp=exp)
	
	# convert our points to Mercatur projection
	x.M <- spTransform(x, CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs'))
	
	# plot google map, and our point of interest
	plot(g, interpolate=TRUE)
	plot(x.M, col=rgb(1, 0, 0, alpha=0.5), add=TRUE)
}



# get pre-cached series extent GeoJSON from SoilWeb server
seriesExtent <- function(s, timeout=60) {
  if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
  
  # encode series name
  s <- gsub(pattern=' ', replacement='_', x=tolower(s))
  
  # base URL to cached data
  u <- URLencode(paste0('http://casoilresource.lawr.ucdavis.edu/series-extent-cache/json/', s, '.json'))
  
  # init temp files / dirs
  td <- tempdir()
  tf.json <- tempfile(fileext='json')
  
  # download GeoJSON file
  download.file(url=u, destfile=tf.json, extra=c(timeout=timeout), quiet=TRUE)
  
  # load into sp object and clean-up
  x <- rgdal::readOGR(dsn=tf.json, layer='OGRGeoJSON', verbose=FALSE)
  unlink(tf.json)
  
  # return in WGS84 GCS
  return(x)
}




