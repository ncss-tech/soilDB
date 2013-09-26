
# experimental function for getting basic KSSL data from CASRL
fetchKSSL <- function(series=NULL, bbox=NULL) {
	
	# sanity-check: user must supply some kind of criteria
	if(missing(series) & missing(bbox))
		stop('you must provide some filtering criteria')
	
	# init empty filter
	f <- vector()
	
	# init empty pieces
	s <- NULL
	h <- NULL
	
	# process filter components
	if(!missing(series)) {
		f <- c(f, paste('&series=', series, sep=''))
	}
	
	if(!missing(bbox)) {
		bbox <- paste(bbox, collapse=',')
		f <- c(f, paste('&bbox=', bbox, sep=''))
	}
	
	# combine filters
	f <- paste(f, collapse='')
	
	# build URLs
	site.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?what=site', f, sep=''))
	hz.url <- url(paste('http://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?what=horizon', f, sep=''))
	
	# load pieces
	try(s <- read.table(site.url, header=TRUE, sep='|', stringsAsFactors=FALSE, quote='', comment=''), silent=TRUE)
	try(h <- read.table(hz.url, header=TRUE, sep='|', stringsAsFactors=FALSE, quote='', comment=''), silent=TRUE)
	
	# report missing data
	if(all(c(is.null(s), is.null(h)))) {
		stop('query returned no data', call.=FALSE)
	}
	
	# upgrade to SoilProfileCollection
	depths(h) <- pedon_key ~ hzn_top + hzn_bot
	site(h) <- s
	
	# pack into a list and return
	return(h)
}
