

# 2011-06-22
# It appears that SDA does not actually return the spatial intersection of map unit polygons and bounding box. Rather, just those polygons that overlap the bbox.
#' Fetch Map Unit Geometry from SDA
#' 
#' @description Fetch map unit geometry from the SDA website by WGS84 bounding box. There is a limit on the amount of data returned as serialized JSON (~32Mb) and a total record limit of 100,000.
#' 
#' @param bbox 	a bounding box in WGS coordinates
#' @param source the source database, currently limited to soil data access (SDA)
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}. See examples for bounding box formatting.
#' @return A SpatialPolygonsDataFrame of map unit polygons, in WGS84 (long,lat) coordinates.
#' @note SDA does not return the spatial intersection of map unit polygons and bounding box. Rather, just those polygons that are completely within the bounding box / overlap with the bbox. This function requires the 'rgdal' package.
#' 
#' @author Dylan E. Beaudette
#' @export
#'
#' @examples
#'## fetch map unit geometry from a bounding-box:
#'# 
#'#         +------------- (-120.41, 38.70)
#'#         |                     |
#'#         |                     |
#'# (-120.54, 38.61) --------------+
#'# 
#' \donttest{
#' if(requireNamespace("curl") &
#' curl::has_internet() &
#'   require(sp) & 
#'   require(rgdal)) {
#'     
#'     # basic usage
#'     b <- c(-120.54,38.61,-120.41,38.70)
#'     x <- try(mapunit_geom_by_ll_bbox(b)) # about 20 seconds
#'     
#'     if(!inherits(x,'try-error')) {
#'       # note that the returned geometry is everything overlapping the bbox
#'       # and not an intersection... why?
#'       plot(x)
#'     rect(b[1], b[2], b[3], b[4], border='red', lwd=2)
#'     
#'     
#'     # get map unit data for matching map unit keys
#'     in.statement <- format_SQL_in_statement(unique(x$mukey))
#'     
#'     q <- paste("SELECT mukey, muname FROM mapunit WHERE mukey IN ", in.statement, sep="")
#'     res <- SDA_query(q)
#'     
#'     #inspect
#'     head(res)
#'   } else {
#'     message('could not download XML result from SDA')
#'   }
#'  }
#'}
mapunit_geom_by_ll_bbox <- function(bbox, source='sda') {
	
	# must have rgdal installed
   if(!requireNamespace('rgdal', quietly=TRUE))
    stop('please install the `rgdal` package', call.=FALSE)
	
	# parse bbox
	bbox.text <- paste(bbox, collapse=',')
	
	# get available OGR drivers
	ogr.Drv <- rgdal::ogrDrivers()$name
	
	# getting data from SDA
	if(source == 'sda') {
		# check to make sure the user's OGR has been compiled with GML support:
		if( ! "GML" %in% ogr.Drv)
			stop('GML support is missing from your GDAL/OGR build.', call.=FALSE)
		
		# compose URL
		u <- paste( 'https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=MapunitPoly&BBOX=', bbox.text, sep='')
		
		# file extension for later
		file.extension <- '.gml'
		
		# layername for later
		file.layer <- 'mapunitpoly'
	}
	
	if(source == 'soilweb') {
		# soilweb emits mixed-geometry KML, OGR can't deal with this
		# ... need a new strategy
		stop('Data from SoilWeb is currently not supported.', call.=FALSE)
		
		# check to make sure the user's OGR has been compiled with GML support:
		if( ! "KML" %in% ogr.Drv)
			stop('KML support is missing from your GDAL/OGR build.', call.=FALSE)
		
		# parse URL
		u <- paste( 'https://casoilresource.lawr.ucdavis.edu/soil_web/export.php?format=kml&srid=4326&BBOX=', bbox.text, sep='')
		
		# file extension for later
		file.extension <- '.kml'
		
		# layername for later
		file.layer <- 'Soil Polygons'
	}
	
	# get a temp location to save the file
	td <- tempdir()
	tf <- tempfile(pattern="file", tmpdir=td)
	tf.full <- paste(tf, file.extension, sep='')
	
	# save the file locally
	ddf <- try(download.file(url=u, destfile=tf.full, quiet=FALSE))
	if (inherits(ddf, "try-error"))
	 return(ddf)
	
	# read in via OGR, into a SPolyDF. 
	# note hard-coded layer name from within the GML source
	# disambiguateFIDs=TRUE is required due to sloppy GML from SDA
	d <- try(rgdal::readOGR(dsn=tf.full, layer=file.layer, disambiguateFIDs=TRUE, stringsAsFactors=FALSE))

	if (inherits(d, "try-error"))
	  return(d)
	
	# throw-out some garbage columns
	d$gml_id <- NULL
	gc()
	
	# done, clean-up
	# leaves the .gfs file hanging around
	unlink(tf.full)
	
	return(d)
}


