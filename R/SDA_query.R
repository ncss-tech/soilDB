
# generate chunk labels for splitting data
# ids: vector of IDs
# size: chunk size
makeChunks <- function(ids, size=100) {
  n <- length(ids)
  chunk.id <- seq(from=1, to=floor(n / size)+1)
  chunk.ids <- rep(chunk.id, each=size)
  chunk.ids <- chunk.ids[1:n]
  return(chunk.ids)
}



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
  
   # submit request
  r <- httr::POST(url = "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest",
                  body = list(query = q,
                              format = "JSON+COLUMNNAME"),
                  encode = "form")
  
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
  # note: the data returned by SDA/JSON are all character class
  #       we "fix" this later on
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
  # * consistent encoding of NA
  # * type conversion via read.table() 
  # * no conversion strings -> factors: do this on your own
  d <- lapply(d, .post_process_SDA_result_set)
  
  # keep track of SDA result set IDs
  SDA.ids <- names(d)
  for(i in 1:n.tables) {
    attr(d[[i]], 'SDA_id') <- SDA.ids[i]
  }
  
  
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
# no conversion of strings -> factors
.post_process_SDA_result_set <- function(i) {
  # the first line is always the colnames
  colnames(i) <- i[1, ]

  # remove the first line
  # Arrg! the dreaded single-row indexing bug: drop=FALSE ensures result is a matrix
  i <- i[-1, , drop=FALSE]
  
  # keep everything in memory, c/o Kyle Bockinsky
  df <- as.data.frame(i, stringsAsFactors = FALSE)
  # attempt type conversion, same result as writing to file and reading-in via read.table()
  df <- type.convert(df,
                     na.strings = c('', 'NA'),
                     as.is = TRUE
                     )
  
  ## TODO further error checking?
  
  # done
  return(df)
}



