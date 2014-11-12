# get the series extent fromSEE pre-cached GeoJSON data and plot on Google Maps
seriesExtentAsGmap <- function(s, timeout=60, exp=1.25) {
  if(!require(dismo))
    stop('please install the `dismo` package', call.=FALSE)
  
	# load series extent data in WGS84 GCS
	x <- seriesExtent(s, timeout)
	
	# make extent object around sites, in geographic coordinates
	e <- extent(spTransform(x, CRS('+proj=longlat')))
	
	# grab ref. to google maps
	g <- gmap(e, exp=exp)
	
	# convert our points to Mercatur projection
	x.M <- spTransform(x, CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs'))
	
	# plot google map, and our point of interest
	plot(g, interpolate=TRUE)
	plot(x.M, col=rgb(1, 0, 0, alpha=0.5), add=TRUE)
}


# get the series extent from SoilWeb KMZ service and return as SpatialPolygonsDataFrame
# seriesExtent <- function(s, timeout=60) {
# 	
# 	# make URL to SoilWeb KMZ
# 	u <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=soil_series_extent&q_string=', s, '&query_level=mapunit', sep=''))
# 	
# 	# init temp files / dirs
# 	td <- tempdir()
# 	tf.kmz <- tempfile(fileext='kmz')
#   
# 	# note that we are transfering as binary (KMZ), extend timeout if needed
# 	download.file(url=u, destfile=tf.kmz, mode='wb', extra=c(timeout=timeout))
# 	
# 	# readOGR can't open KMZ files directly, so we must unzip
# 	unzip(zipfile=tf.kmz, files='doc.kml', exdir=td)
# 	
# 	# load into sp object and clean-up
# 	x <- readOGR(dsn=file.path(td, 'doc.kml'), layer='Soil Series Extent')
# 	unlink(tf.kmz)
# 	
# 	# return in WGS84 GCS
# 	return(x)
# }


# get pre-cached series extent GeoJSON from SoilWeb server
seriesExtent <- function(s, timeout=60) {
  # encode series name
  s <- gsub(pattern=' ', replacement='_', x=tolower(s))
  
  # base URL to cached data
  u <- URLencode(paste0('http://casoilresource.lawr.ucdavis.edu/series-extent-cache/json/', s, '.json'))
  
  # init temp files / dirs
  td <- tempdir()
  tf.json <- tempfile(fileext='json')
  
  # download GeoJSON file
  download.file(url=u, destfile=tf.json, extra=c(timeout=timeout), quiet=TRUE)
  
  ogrListLayers(tf.json)
  
  # load into sp object and clean-up
  x <- readOGR(dsn=tf.json, layer='OGRGeoJSON', verbose=FALSE)
  unlink(tf.json)
  
  # return in WGS84 GCS
  return(x)
}




