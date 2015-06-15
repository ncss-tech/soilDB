## NOTE: this function assumes that the series name in osd.osd_colors exactly matches seriesname in osd.taxa

# fetch basic OSD data from the SoilWeb snapshot of the SC database
fetchOSD <- function(soils) {
	
	# base URLs
	u.osd_site <- 'http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=osd_site_query&q_string='
	u.osd_hz <- 'http://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=osd_query&q_string='
	
	# compile URL + requested soil series
  # note: not affected by horizon names with prime (')
	u.site <- paste(u.osd_site, paste(soils, collapse=','), sep='')
	u.hz <- paste(u.osd_hz, paste(soils, collapse=','), sep='')
	
	# encode special characters into URLS
	u.site <- URLencode(u.site)
	u.hz <- URLencode(u.hz)
	
	# init empty vars
	s <- NULL
	h <- NULL
	
	# request data
	try(s <- read.csv(url(u.site), stringsAsFactors=FALSE), silent=TRUE)
	try(h <- read.csv(url(u.hz), stringsAsFactors=FALSE), silent=TRUE)
	
	# report missing data
	if(any(c(is.null(s), is.null(h)))) {
		stop('query returned no data', call.=FALSE)
	}
	
	# reformatting and color conversion
	h$soil_color <- with(h, munsell2rgb(matrix_wet_color_hue, matrix_wet_color_value, matrix_wet_color_chroma))
	h <- with(h, data.frame(id=series, top, bottom, hzname, soil_color, 
													hue=matrix_wet_color_hue, value=matrix_wet_color_value, 
													chroma=matrix_wet_color_chroma, stringsAsFactors=FALSE))
	
	# upgrade to SoilProfileCollection
	depths(h) <- id ~ top + bottom
	
	# merge-in site data
	s$id <- s$seriesname
	s$seriesname <- NULL
	site(h) <- s
	
	# done
	return(h)
}
