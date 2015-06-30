

# 2011-06-22
# It appears that SDA does not actually return the spatial intersecion of map unit polygons and bounding box. Rather, just those polygons that overlap the bbox.
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
		u <- paste( 'http://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=MapunitPoly&BBOX=', bbox.text, sep='')
		
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
		u <- paste( 'http://casoilresource.lawr.ucdavis.edu/soil_web/export.php?format=kml&srid=4326&BBOX=', bbox.text, sep='')
		
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
	download.file(url=u, destfile=tf.full, quiet=FALSE)
	
	# read in via OGR, into a SPolyDF. 
	# note hard-coded layer name from within the GML source
	# disambiguateFIDs=TRUE is required due to sloppy GML from SDA
	d <- rgdal::readOGR(dsn=tf.full, layer=file.layer, disambiguateFIDs=TRUE, stringsAsFactors=FALSE)
	
	# throw-out some garbage columns
	d$gml_id <- NULL
	gc()
	
	# done, clean-up
	# leaves the .gfs file hanging around
	unlink(tf.full)
	
	return(d)
}


