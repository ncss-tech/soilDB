
# experimental function for getting basic KSSL data from CASRL
fetchKSSL <- function(series=NULL, bbox=NULL, mlra=NULL) {
	
	# sanity-check: user must supply some kind of criteria
	if(missing(series) & missing(bbox) & missing(mlra))
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
	
	if(!missing(mlra)) {
	  mlra <- paste(mlra, collapse=',')
	  f <- c(f, paste('&mlra=', mlra, sep=''))
	}
  
	# combine filters
	f <- paste(f, collapse='')
	
	# build URLs
	site.url <- URLencode(paste0('http://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?gzip=1&what=site', f))
	hz.url <- URLencode(paste0('http://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?gzip=1&what=horizon', f))
	
  # init temp files
	tf.site <- tempfile()
	tf.hz <- tempfile()
	
	# download pieces
	download.file(url=site.url, destfile=tf.site, mode='wb', quiet=TRUE)
	download.file(url=hz.url, destfile=tf.hz, mode='wb', quiet=TRUE)
  
  
	# load pieces
  # note use quote="" to ignore prime symbols (') in horizon names
	try(s <- read.table(gzfile(tf.site), header=TRUE, sep='|', stringsAsFactors=FALSE, quote='', comment.char=''), silent=TRUE)
	try(h <- read.table(gzfile(tf.hz), header=TRUE, sep='|', stringsAsFactors=FALSE, quote='', comment.char=''), silent=TRUE)
  
	# report missing data
	if(all(c(is.null(s), is.null(h)))) {
		stop('query returned no data', call.=FALSE)
	}
	
  
	# upgrade to SoilProfileCollection
	depths(h) <- pedon_key ~ hzn_top + hzn_bot
	site(h) <- s
	
	# pack into a list for the user
	res.size <- round(object.size(h) / 1024 / 1024, 2)
	
	# some feedback via message:
	message(paste(length(h), ' pedons loaded (', res.size, ' Mb transferred)', sep=''))
  
	# return assembled SPC
	return(h)
}
