


# helper function for processing WKT returned by SDA_query()
# result is an SPDF
# d: data frame
# g: column containing WKT
# p4s: PROJ4 CRS defs
## TODO: test with geom appearing in different positions within query results
processSDA_WKT <- function(d, g='geom', p4s='+proj=longlat +datum=WGS84') {
  # iterate over features (rows) and convert into list of SPDF
  p <- list()
  n <- nrow(d)
  # pb <- txtProgressBar(style = 3)
  for(i in seq(1, n)) {
    # extract the current row in the DF
    d.i <- d[i, ] 
    # extract the current feature from WKT
    p.i <- rgeos::readWKT(d.i[[g]], id = i, p4s = p4s)
    # remove geom from current row of DF
    d.i[[g]] <- NULL
    # compose SPDF, with other attributes
    s.i <- SpatialPolygonsDataFrame(p.i, data=cbind(data.frame(gid=i, stringsAsFactors = FALSE), d.i), match.ID = FALSE)
    # fix column names
    names(s.i) <- c('gid', names(d.i))
    # save to list
    p[[i]] <- s.i
    # setTxtProgressBar(pb, i/n)
  }
  # close(pb)
  
  # reduce list to single SPDF
  spdf <- do.call('rbind', p)
  
  return(spdf)
}




# i is a single Spatial* object with CRS: WGS84 GCS
SDA_make_spatial_query <- function(i) {
  
  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # convert single feature to WKT
  i.wkt <- rgeos::writeWKT(i)
  
  # programatically generate query
  q <- paste0("SELECT mukey, muname
              FROM mapunit
              WHERE mukey IN (
              SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", i.wkt, "')
              )")
  
  # send query
  res <- SDA_query(q)
  
  # check for no data
  if(is.null(res))
    res <- NA
  
  # done
  return(res)
}

# x is a Spatial* object with more than 1 feature
# id is the name of an attribute that contains a unique ID for each feature
SDA_query_features <- function(x, id='pedon_id') {
  
  # sanity check: ensure that the ID is unique
  if(length(x[[id]]) != length(unique(x[[id]])))
    stop('id is not unique')
  
  # transform to GCS WGS84
  x <- spTransform(x, CRS('+proj=longlat +datum=WGS84'))
  
  # iterate over features and save to list
  l <- list()
  n <- length(x)
  # setup a progress bar for timing
  pb <- txtProgressBar(max=n, style=3)
  for(i in 1:n) {
    # make query
    res <- SDA_make_spatial_query(x[i, ])
    # save results along with an ID
    res <- cbind(id=x[[id]][i], res, stringsAsFactors = FALSE)
    names(res) <- c(id, 'mukey', 'muname')
    l[[i]] <- res
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # convert to data.frame, there may be > 1 row / feature when using lines / polygons
  d <- ldply(l)
  return(d)
}



# format vector of values into a string suitable for an SQL `IN` statement
# currently expects character data only
format_SQL_in_statement <- function(x) {
	i <- paste(x, collapse="','")
	i <- paste("('", i, "')", sep='')
	return(i)
}

## TODO: parse multiple record sets, return as list... currently results are combined into a single DF
##         SDA_query("select top 3 areasymbol from mupoint; select top 2 lkey from mapunit")
## TODO: doesn't close all connections
## TODO: requires more testing and error-trapping
SDA_query <- function(q) {
  # check for required packages
  if(!requireNamespace('httr', quietly=TRUE) | !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)
  
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
  r <- httr::POST(url="http://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest", body=httr::upload_file(tf.1))
  httr::stop_for_status(r)
  
  ## httr 1.1.0: content now returns an xml_document
  # bug fix suggested by Kyle Bocinsky, thanks!
  # extract content as XML
  r.content <- httr::content(r, as = 'text', encoding = 'UTF-8')
  d <- xmlToDataFrame(r.content, stringsAsFactors = FALSE)
  
  # how many lines of output
  lines.of.data <- nrow(d)
  
  # the first line is garbage, unless there is an error
  if(lines.of.data > 1)
    d <- d[-1, ]
  
  # error condition
  if(lines.of.data == 1) {
    stop(paste0('SDA returned an error: ', unlist(d)), call. = FALSE)
  }
  
  # check for no returned data, 'd' will be a character object with 0 elements
  if(class(d) == 'character') {
    message('query returned 0 rows')
    return(NULL)
  }
  
  # first column is garbage
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


### this is the old-style interface, now depreciated ###
# 
# # clean-up results from SDA SOAP query, and return as DF
# .cleanSDA <- function(i) {
#   # remove left-overs from SOAP result
#   i$.attrs <- NULL
#   
#   # convert NULL in NA
#   i[which(sapply(i, is.null))] <- NA
#   
#   # convert list to DF
#   return(as.data.frame(i, stringsAsFactors=FALSE))
# }
# 
# .oldSDA_query <- function(q) {
#   # check for required packages
#   if(!requireNamespace('SSOAP', quietly=TRUE) | !requireNamespace('XMLSchema', quietly=TRUE))
#     stop('please install the `SSOAP` and `XMLSchema` packages', call.=FALSE)
#     
#   # important: change the default behavior of data.frame
# 	opt.original <- options(stringsAsFactors = FALSE)
# 	
# 	# setup server, action, and xmlns
# 	s <- SSOAP::SOAPServer('SDMDataAccess.nrcs.usda.gov', '/Tabular/SDMTabularService.asmx')
# 	a <- I('http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery')
# 	x <- c(I("http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx"))
# 	
# 	# feedback:
# 	cat('sending SOAP request...\n')
# 	
# 	# submit and process the query
# 	res <- SSOAP::.SOAP(s, "RunQuery", Query=q, action=a, xmlns=x)
# 	
# 	# results are stored in: res$diffgram$NewDataSet
# 	
# 	# clean the results, convert to DF
# 	cat('processing results...\n')
# 	
# 	df <- ldply(res$diffgram$NewDataSet, .fun=.cleanSDA, .progress='text')
# 	df$.id <- NULL
# 	
# 	# temp hack: everything is read-in as character data!!
# 	# write out to tempfile, and read back in
# 	f <- tempfile()
# 	write.table(df, file=f, col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
# 	df <- read.table(f, header=TRUE, sep='|', quote='', comment.char='')
# 	
# 	# reset options:
# 	options(opt.original)
# 	
# 	# done
# 	return(df)
# }


