# format vector of values into a string suitable for an SQL `IN` statement
# currently expects character data only
format_SQL_in_statement <- function(x) {
	i <- paste(x, collapse="','")
	i <- paste("('", i, "')", sep='')
	return(i)
}


## chunked queries for large number of records:
# https://github.com/ncss-tech/soilDB/issues/71

# 
SDA_query <- function(q) {
  # check for required packages
  if(!requireNamespace('httr', quietly=TRUE) | !requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `httr` and `jsonlite` packages', call.=FALSE)
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)
  
  # temp place to keep json-style post args
  tf <- tempfile() 
  
  # compute json post args and save to temp file
  # result is JSON with first-line as column names
  # this means result is a character matrix
  post.data <- jsonlite::toJSON(list(query=q, format='JSON+COLUMNNAME'), auto_unbox = TRUE)
  cat(post.data, file=tf, sep = '\n')
  
  # submit request
  r <- httr::POST(url="https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest", body=httr::upload_file(tf))
  
  # trap errors, likely related to SQL syntax errors
  request.status <- try(httr::stop_for_status(r), silent = TRUE)
  
  # error message is encapsulated in XML, use xml2 library functions to extract
  if(class(request.status) == 'try-error'){
    # get the request response, this will contain an error message
    r.content <- httr::content(r, as = 'parsed', encoding = 'UTF-8')
    # parse the XML to get the error message
    error.msg <- xml_text(r.content)
    
    ## TODO: error or message?
    stop(error.msg)
  }
  
  
  # the result is JSON:
  # list of character matrix, one for each "Table" returned
  r.content <- httr::content(r, as = 'text', encoding = 'UTF-8')
  d <- jsonlite::fromJSON(r.content)
  
  # number of results
  n.tables <- length(d)
  
  # no results, terminate here
  if(n.tables < 1) {
    message('empty result set')
    return(NULL)
  }
  
  # process list of tables
  d <- lapply(d, .post_process_SDA_result_set)
  
  # keep track of SDA result set IDs
  SDA.ids <- names(d)
  for(i in 1:n.tables) {
    attr(d[[i]], 'SDA_id') <- SDA.ids[i]
  }
  
  # reset options
  options(opt.original)
  
  if(n.tables > 1) {
    message('multi-part result set, returning a list')
    return(d)
  } else {
    # single result set, returng data.frame
    message('single result set, returning a data.frame')
    return(d[[1]])
  }
    
  
  
  
  
}


# note: empty strings and 'NA' are converted into <NA>
# convert the raw results from SDA into a proper data.frame
.post_process_SDA_result_set <- function(i) {
  # the first line is always the colnames
  i.header <- i[1, ]
  
  # remove the first line
  # Arrg! the dreaded sing-row indexing bug: drop=FALSE ensures result is a matrix
  i <- i[-1, , drop=FALSE]
  
  i.tf <- tempfile() # work-around for all data encoded as char
  
  # save to file / re-load to guess column classes
  write.table(i, file=i.tf, col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
  
  ## https://github.com/ncss-tech/soilDB/issues/28
  ## this breaks when there are multi-line records
  df <- read.table(i.tf, header=TRUE, sep='|', quote='', comment.char='', na.strings = c('', 'NA'), stringsAsFactors = FALSE)
  
  ## not quite there...
  # tmp <- scan(file=i.tf, multi.line = TRUE, blank.lines.skip = TRUE, what=list(rep(character(), times=length(i.header))), sep='\n', quote='', comment.char='', quiet = TRUE, na.strings = c('', 'NA'), skip=1)
  
 
  # add colnames from original header
  names(df) <- i.header
  
  ## error checking?
  
  # done
  return(df)
}



