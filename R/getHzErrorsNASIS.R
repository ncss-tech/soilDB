## TODO: this isn't really needed any more
getHzErrorsNASIS <- function(strict=TRUE) {
	
	# get data
	site_data <- get_site_data_from_NASIS_db()
	hz_data <- get_hz_data_from_NASIS_db()
	
	# combine pieces
	f <- join(hz_data, site_data, by='peiid', type='inner')
	
	# ignore missing lower boundary
	f.test <- ddply(f, 'pedon_id', function(d) {
	  res <- aqp::hzDepthTests(top=d[['hzdept']], bottom=d[['hzdepb']])
	  return(data.frame(hz_logic_pass=all(!res)))
	})
	
	# find bad ones
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# now describe the problems
	b <- f[which(f$pedon_id %in% bad.pedon.ids), c('peiid', 'pedon_id','hzdept','hzdepb','hzname')]
	
	return(b) 
	
}
