# get the series extent from SoilWeb KMZ service and plot on Google Maps
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
seriesExtent <- function(s, timeout=60) {
	if(!require(rgdal))
		stop('please install the `rgdal` package', call.=FALSE)
	
	# make URL to SoilWeb KMZ
	u <- URLencode(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=soil_series_extent&q_string=', s, '&mode=bbox&query_level=mapunit&format=kmz&', sep=''))
	
	# save result as tempfile
	tf <- tempfile()
	
	# note that we are transfering as binary (KMZ), extend timeout if needed
	download.file(url=u, destfile=tf, mode='wb', extra=c(timeout=timeout))
	
	# load into sp object and clean-up
	x <- readOGR(dsn=tf, layer='Soil Series Extent')
	unlink(tf)
	
	# return in WGS84 GCS
	return(x)
}