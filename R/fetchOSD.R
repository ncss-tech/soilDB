
# 2018-10-11: updated to new API, URL subject to change
# fetch basic OSD, SC, and SoilWeb summaries from new API
fetchOSD <- function(soils, colorState='moist', extended=FALSE) {
	
  # sanity check
  if( !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` package', call.=FALSE)
  
  ## TODO: URLs will change once API goes live
  # compose query
  if(extended) {
    url <- 'http://soilmap2-1.lawr.ucdavis.edu/dylan/soilweb/api/soil-series.php?q=all&s='
  } else {
    url <- 'http://soilmap2-1.lawr.ucdavis.edu/dylan/soilweb/api/soil-series.php?q=site_hz&s='
  }
  
  # format series list and append to url
  final.url <- paste(url, URLencode(paste(soils, collapse=',')), sep='')
  
  # attempt query to API, result is JSON
  res <- try(jsonlite::fromJSON(final.url))
  
  ## TODO: further testing / message detail required
  # trap errors
  if(class(res) == 'try-error'){
    message('error')
    return(NULL)
  }
  
  # extract site+hz data
  # these will be FALSE if query returns NULL
  s <- res$site
  h <- res$hz
  
	# report missing data
  # no data condition: s == FALSE | h == FALSE
  # otherwise both will be a data.frame
	if( (is.logical(s) & length(s) == 1) | (is.logical(h) & length(s) == 1)) {
		message('query returned no data')
	  return(NULL)
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
	
	# standard or extended version?
	if(extended) {
	  # extended
	  # compose into a list
	  data.list <- list(
	    SPC=h,
	    geomcomp=res$geomcomp,
	    hillpos=res$hillpos,
	    mtnpos=res$mtnpos,
	    pmkind=res$pmkind,
	    pmorigin=res$pmorigin,
	    mlra=res$mlra
	  )
	  
	  return(data.list)
	  
	} else {
	  # standard
	  return(h) 
	}

	
}
