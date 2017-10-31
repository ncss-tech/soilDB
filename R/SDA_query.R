
### TODO: re-name these functions with more informative labels



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


## TODO: this is likely too slow...
## TODO: this new function will return intersected geometry data
# i is a single Spatial* object with CRS: WGS84 GCS
SDA_make_spatial_query2 <- function(i) {
  
  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # convert single feature to WKT
  i.wkt <- rgeos::writeWKT(i)
  
  q <- paste0("
-- setup aoi from single WKT feature
~DeclareGeometry(@aoi)~
select @aoi = geometry::STPolyFromText('", i.wkt, "', 4326)

-- Extract all intersected polygons
~DeclareIdGeomTable(@intersectedPolygonGeometries)~
~GetClippedMapunits(@aoi,polygon,geo,@intersectedPolygonGeometries)~

-- Convert geometries to geographies so we can get areas
~DeclareIdGeogTable(@intersectedPolygonGeographies)~

~GetGeogFromGeomWgs84(@intersectedPolygonGeometries,@intersectedPolygonGeographies)~

-- Return the polygonal geometries
select * from @intersectedPolygonGeographies where geog.STGeometryType() = 'Polygon';

-- get aggregated areas and associated mukey, musym, nationalmusym, areasymbol, mucertstat (Map Unit Certification Status)
-- select id, sum(geog.STArea()) as area
-- into #aggarea from @intersectedPolygonGeographies
-- group by id;

-- Return the polygons with joined data
-- select mukey, area, musym, nationalmusym, areasymbol, mucertstat
-- from #aggarea A, mapunit M, legend L
-- where A.id = M.mukey and M.lkey = L.lkey
-- order by id;

-- Return the aggregated area by mukey
-- select id as mukey, area from #aggarea
")
  
  # send query
  res <- SDA_query(q)
  
  # check for no data
  if(is.null(res))
    res <- NA
  
  # done
  return(res)
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
              SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('", i.wkt, "')
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



