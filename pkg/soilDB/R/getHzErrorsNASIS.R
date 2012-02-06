getHzErrorsNASIS <- function() {
	
	# get data
	site_data <- get_site_data_from_pedon_db()
	hz_data <- get_hz_data_from_pedon_db()
	
	# combine pieces
	f <- join(hz_data, site_data, by='peiid', type='inner')
	
	# ignore missing lower boundary
	f.test <- ddply(f, .(pedon_id), test_hz_logic, topcol='hzdept', bottomcol='hzdepb', test.NA=FALSE, strict=TRUE)
	
	# find bad ones
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# now describe the problems
	b <- subset(f, select=c('pedon_id','hzdept','hzdepb','hzname'), subset=pedon_id %in% bad.pedon.ids)
	
	return(b) 
	
}
