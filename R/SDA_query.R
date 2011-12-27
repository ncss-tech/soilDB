## why doesn't this work ???
# p <- processWSDL('http://sdmdataaccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx?WSDL')

# format vector of values into a string suitable for an SQL `IN` statement
# currently expects character data only
format_SQL_in_statement <- function(x)
  {
  i <- paste(x, collapse="','")
  i <- paste("('", i, "')", sep='')
  return(i)
  }

# clean-up results from SDA SOAP query, and return as DF
cleanSDA <- function(i) 
  {
  # important: change the default behavior of data.frame and melt
  opt.original <- options(stringsAsFactors = FALSE)
  
  # remove left-overs from SOAP result
  i$.attrs <- NULL
  
  # convert NULL in NA
  i[which(sapply(i, is.null))] <- NA
  
  # reset options:
  options(opt.original)
  
  # convert list to DF
  return(as.data.frame(i))
  }

# TODO: figure out how to inspect the results and set column classes
SDA_query <- function(q)
  {
  # check for required packages
  if(!require(SSOAP))
    stop('please install the `SSOAP` package')
    
  # setup server, action, and xmlns
  s <- SOAPServer('SDMDataAccess.nrcs.usda.gov', '/Tabular/SDMTabularService.asmx')
  a <- I('http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery')
  x <- c(I("http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx"))
  
  # feedback:
  cat('sending SOAP request...\n')
  
  # submit and process the query
  res <- .SOAP(s, "RunQuery", Query=q, action=a, xmlns=x)
  
  # results are stored in: res$diffgram$NewDataSet
  
  # clean the results, convert to DF
  cat('processing results...\n')
  
  df <- ldply(res$diffgram$NewDataSet, .fun=cleanSDA, .progress='text')
  df$.id <- NULL
  
  # temp hack: everything is read-in as character data!!
  # write out to tempfile, and read back in
  f <- tempfile()
  write.table(df, file=f, col.names=TRUE, row.names=FALSE, quote=FALSE, sep='|')
  df <- read.table(f, header=TRUE, as.is=TRUE, sep='|')
  
  # done
  return(df)
  }


