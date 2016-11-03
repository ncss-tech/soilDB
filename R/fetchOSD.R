## NOTE: this function assumes that the series name in osd.osd_colors exactly matches seriesname in osd.taxa

# fetch basic OSD data from the SoilWeb snapshot of the SC database
fetchOSD <- function(soils, colorState='moist') {
	
	# base URLs
	u.osd_site <- 'https://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=osd_site_query&q_string='
	u.osd_hz <- 'https://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=osd_query&q_string='
	
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
	if(colorState == 'moist') {
	  h$soil_color <- with(h, munsell2rgb(matrix_wet_color_hue, matrix_wet_color_value, matrix_wet_color_chroma))
	  h <- with(h, data.frame(id=series, top, bottom, hzname, soil_color, 
	                          hue=matrix_wet_color_hue, value=matrix_wet_color_value, 
	                          chroma=matrix_wet_color_chroma, dry_hue=matrix_dry_color_hue,
	                          dry_value=matrix_dry_color_value, dry_chroma=matrix_dry_color_chroma,
	                          texture_class=texture_class, cf_class=cf_class, pH=ph, pH_class=ph_class,
	                          narrative=narrative,
	                          stringsAsFactors=FALSE)) 
	}
	
	if(colorState == 'dry') {
	  h$soil_color <- with(h, munsell2rgb(matrix_dry_color_hue, matrix_dry_color_value, matrix_dry_color_chroma))
	  h <- with(h, data.frame(id=series, top, bottom, hzname, soil_color, 
	                          hue=matrix_dry_color_hue, value=matrix_dry_color_value, 
	                          chroma=matrix_dry_color_chroma, moist_hue=matrix_wet_color_hue,
	                          moist_value=matrix_wet_color_value, moist_chroma=matrix_wet_color_chroma,
	                          texture_class=texture_class, cf_class=cf_class, pH=ph, pH_class=ph_class,
	                          narrative=narrative,
	                          stringsAsFactors=FALSE))
	}
	
	
	# upgrade to SoilProfileCollection
	depths(h) <- id ~ top + bottom
	
	## borrowed from OSD parsing code
	## TODO: merge into aqp
	textures <- c('coarse sand', 'sand', 'fine sand', 'very fine sand', 'loamy coarse sand', 'loamy sand', 'loamy fine sandy', 'loamy very fine sand', 'coarse sandy loam', 'sandy loam', 'fine sandy loam', 'very fine sandy loam', 'loam', 'silt loam', 'silt', 'sand clay loam', 'clay loam', 'silty clay loam', 'sandy clay', 'silty clay', 'clay')
	pH_classes <- c('ultra acid', 'extremely acid', 'vert strongly acid', 'strongly acid', 'moderately acid', 'slightly acid', 'neutral', 'slightly alkaline', 'mildly alkaline', 'moderately alkaline', 'strongly alkaline', 'very strongly alkaline')
	
	# convert some columns into factors
	h$texture_class <- factor(h$texture_class, levels=textures, ordered = TRUE)
	h$pH_class <- factor(h$pH_class, levels=pH_classes, ordered = TRUE)
	
	# merge-in site data
	s$id <- s$seriesname
	s$seriesname <- NULL
	site(h) <- s
	
	# done
	return(h)
}
