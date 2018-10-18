
## chunked queries for large number of records:
# https://github.com/ncss-tech/soilDB/issues/71

## TODO items summarized here
# https://github.com/ncss-tech/soilDB/issues/81



# helper function for processing WKT returned by SDA_query()
# result is an SPDF
# d: data frame
# g: column containing WKT
# p4s: PROJ4 CRS defs
## TODO: test with geom appearing in different positions within query results
## TODO: geometry collections are not allowed in sp objects..
## TODO: consider moving to sf
processSDA_WKT <- function(d, g='geom', p4s='+proj=longlat +datum=WGS84') {
  # iterate over features (rows) and convert into list of SPDF
  p <- list()
  n <- nrow(d)
  
  # points or polygons?
  # looking at the first feature for efficiency, all others should be the same
  g.type <- class(rgeos::readWKT(d[1, g]))
  
  # pb <- txtProgressBar(style = 3)
  for(i in seq(1, n)) {
    # extract the current row in the DF
    d.i <- d[i, ] 
    # extract the current feature from WKT
    p.i <- rgeos::readWKT(d.i[[g]], id = i, p4s = p4s)
    # remove geom from current row of DF
    d.i[[g]] <- NULL
    
    # compose SpatialPointsDataFrame, with other attributes
    if(g.type == 'SpatialPoints')
      s.i <- SpatialPointsDataFrame(p.i, data=cbind(data.frame(gid=i, stringsAsFactors = FALSE), d.i), match.ID = FALSE)
    
    # compose SpatialPolygonsDataFrame, with other attributes
    if(g.type == 'SpatialPolygons')
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

## Note: this will replace *most* of the functionality in:
##   SDA_make_spatial_query
##   SDA_query_features
#
# get a SPDF of intersecting MU polygons and mukey, in a single query
# row-order and number of rows won't always match input
# that is fine as we can use sp::over to connect
# 10-20x speed improvement over SDA_query_features
# 
# not a good substitute for small number of features and SDA_query_features()
#
# geom are converted to GCS WGS84 as needed
#
# geom: Spatial* object with valid CRS
# what: the type of query 
# mukey = fast, results include intersecting mukeys
# geom = slower, results include both intersecting geometry and mukeys
SDA_spatialQuery <- function(geom, what='mukey') {
  
  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # sanity checks
  if(! what %in% c('mukey', 'geom')) {
    stop("query type must be either 'mukey' or 'geom'",call. = FALSE)
  }
  
  # CRS conversion if needed
  prj.4326 <- "+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"
  if(proj4string(geom) != prj.4326) {
    geom <- spTransform(geom, CRS(prj.4326))
  }
  
  # WKT encoding
  # use a geometry collection
  wkt <- rgeos::writeWKT(geom, byid = FALSE)
  
  # slower query, returning geom + mukey
  # replacement for depreciated SDA_query_features()
  # 10-30x faster than spatial-returning query by input feature
  # TODO: this is 15x slower than non-spatial-returning-query in SDA_query_features()
  if(what == 'geom') {
    q <- sprintf("
               SELECT 
                 mupolygongeo.STAsText() AS geom, P.mukey
                 FROM mupolygon AS P
                 WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;", wkt)
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
    res <- processSDA_WKT(res)
  }
  
  # faster query, returning mukey + muname
  # replacement for depreciated SDA_make_spatial_query()
  # ~ 3x faster than SDA_query_features()
  # TODO: how can we link these back with the source data?
  if(what == 'mukey') {
    q <- sprintf("SELECT mukey, muname
                FROM mapunit
                WHERE mukey IN (
                SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('%s')
                )", wkt)
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
  }
  
  
  return(res)
}


## TODO: there is a lot of overhead connecting to SDA, large queries are best done in bulk!
# this is intended for those cases where a result is required for each feature processed
# i is a Spatial* object with valid CRS, a single feature or multiple (converted to a geometry collection)
SDA_make_spatial_query <- function(i) {
  
  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # convert single feature to WKT
  i.wkt <- rgeos::writeWKT(i, byid = FALSE)
  
  # programatically generate query
  q <- paste0("SELECT mukey, muname
              FROM mapunit
              WHERE mukey IN (
              SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('", i.wkt, "')
              )")
  
  # send query, messages aren't useful here
  res <- suppressMessages(SDA_query(q))
  
  # check for no data
  if(is.null(res))
    res <- NA
  
  # done
  return(res)
}

# this is a safe way to query features while preserving IDs
# note that it is a very slow for large collections
# x is a Spatial* object with more than 1 feature
# id is the name of an attribute that contains a unique ID for each feature
SDA_query_features <- function(x, id='pedon_id') {
  
  # sanity check: ensure that the ID is unique
  if(length(x[[id]]) != length(unique(x[[id]])))
    stop('id is not unique')
  
  # transform to GCS WGS84 if needed
  prj.4326 <- "+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"
  if(proj4string(x) != prj.4326) {
    geom <- spTransform(x, CRS(prj.4326))
  }
  
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

