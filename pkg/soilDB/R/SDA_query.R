
# format vector of values into a string suitable for an SQL `IN` statement
# currently expects character data only
format_SQL_in_statement <- function(x) {
	i <- paste(x, collapse="','")
	i <- paste("('", i, "')", sep='')
	return(i)
}

# clean-up results from SDA SOAP query, and return as DF
.cleanSDA <- function(i) {
	# remove left-overs from SOAP result
	i$.attrs <- NULL
	
	# convert NULL in NA
	i[which(sapply(i, is.null))] <- NA
	
	# convert list to DF
	return(as.data.frame(i, stringsAsFactors=FALSE))
}

## TODO: requires more testing and error-trapping
SDA_query <- function(q) {
  # check for required packages
  if(!requireNamespace('httr', quietly=TRUE) | !requireNamespace('XML', quietly=TRUE))
    stop('please install the `httr` and `XML` packages', call.=FALSE)
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # need 2 temp files
  tf.1 <- tempfile() # json-style post args
  tf.2 <- tempfile() # work-around for all data encoded as char
  
  # compute json post args and save to temp file
  # note: asking for data to be returned as XML... JSON version doesn't include column names
  post.data <- jsonlite::toJSON(list(query=q, format='xml'), auto_unbox = TRUE)
  cat(post.data, file=tf.1, sep = '\n')
  
  # submit request
  cat('sending POST request...\n')
  r <- httr::POST(url="http://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest", body=httr::upload_file(tf.1))
  httr::stop_for_status(r)
  
  # extract content as XML
  r.content <- httr::content(r)
  d <- xmlToDataFrame(r.content, stringsAsFactors = FALSE)
  
  # first line and column are garbage
  d <- d[-1, ]
  d$element <- NULL
  
  # save to file / re-load to guess column classes
  write.table(d, file=tf.2, col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
  df <- try(read.table(tf.2, header=TRUE, sep='|', quote='', comment.char='', na.strings = ''), silent=TRUE)
  
  if(class(df) == 'try-error')
    stop('invalid query')
  
  # reset options:
  options(opt.original)
  
  return(df)
}

.oldSDA_query <- function(q) {
  # check for required packages
  if(!requireNamespace('SSOAP', quietly=TRUE) | !requireNamespace('XMLSchema', quietly=TRUE))
    stop('please install the `SSOAP` and `XMLSchema` packages', call.=FALSE)
    
  # important: change the default behavior of data.frame
	opt.original <- options(stringsAsFactors = FALSE)
	
	# setup server, action, and xmlns
	s <- SSOAP::SOAPServer('SDMDataAccess.nrcs.usda.gov', '/Tabular/SDMTabularService.asmx')
	a <- I('http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery')
	x <- c(I("http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx"))
	
	# feedback:
	cat('sending SOAP request...\n')
	
	# submit and process the query
	res <- SSOAP::.SOAP(s, "RunQuery", Query=q, action=a, xmlns=x)
	
	# results are stored in: res$diffgram$NewDataSet
	
	# clean the results, convert to DF
	cat('processing results...\n')
	
	df <- ldply(res$diffgram$NewDataSet, .fun=.cleanSDA, .progress='text')
	df$.id <- NULL
	
	# temp hack: everything is read-in as character data!!
	# write out to tempfile, and read back in
	f <- tempfile()
	write.table(df, file=f, col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
	df <- read.table(f, header=TRUE, sep='|', quote='', comment.char='')
	
	# reset options:
	options(opt.original)
	
	# done
	return(df)
}


