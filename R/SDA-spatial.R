
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



## TODO: this will replace *most* of the functionality in:
##   SDA_make_spatial_query
##   SDA_query_features
#
# get a SPDF of intersecting MU polygons and mukey, in a single query
# row-order and number of rows won't always match input
# that is fine as we can use sp::over to connect
# 10-20x speed improvement over SDA_query_features


#' @title SDA Spatial Query
#' 
#' @description Query SDA (SSURGO / STATSGO) records via spatial intersection with supplied geometries. Input can be SpatialPoints, SpatialLines, or SpatialPolygons objects with a valid CRS. Map unit keys, overlapping polygons, or the spatial intersectionion of `geom` + SSURGO polygons can be returned. See details.
#' 
#' @param geom a Spatial* object, with valid CRS. May contain multiple features.
#' @param what a character vector specifting what to return. `mukey`: `data.frame` with intersecting map unit keys and names, `geom` overlapping or intersecting map unit polygons
#' @param geomIntersection logical; FALSE: overlapping map unit polygons returned, TRUE: intersection of `geom` + map unit polygons is returned.
#' 
#' @return A `data.frame` if `what` is 'mukey', otherwise `SpatialPolygonsDataFrame` object.
#' 
#' @author D.E. Beaudette
#' @seealso \code{\link{SDA_query}}
#' @keywords manip
#' 
#' @note Row-order is not preserved across features in `geom` and returned object. Use `sp::over()` or similar functionality to extract from results.
#' 
#' @details Queries for map unit keys are always more efficient vs. queries for overlapping or intersecting (i.e. least efficient) features. `geom` is converted to GCS / WGS84 as needed.
#' 
#' There is a 100,000 record limit and 32Mb JSON serializer limit, per query.
#' 
#' SSURGO (detailed soil survey) and STATSGO (generalized soil survey) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for details.
#' 
#' 
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() & 
#'    requireNamespace("sp")) {
#' 
#'    library(aqp)
#'    library(sp)
#' 
#' # example point
#' p <- SpatialPoints(cbind(x = -119.72330, y = 36.92204), 
#'                          proj4string = CRS('+proj=longlat +datum=WGS84'))
#' 
#' # query map unit records at this point
#' res <- SDA_spatialQuery(p, what = 'mukey')
#' 
#' # convert results into an SQL "IN" statement
#' # useful when there are multiple intersecting records
#' mu.is <- format_SQL_in_statement(res$mukey)
#' 
#' # composite SQL WHERE clause
#' sql <- sprintf("mukey IN %s", mu.is)
#' 
#' # get commonly used map unit / component / chorizon records
#' # as a SoilProfileCollection object
#' # confusing but essential: request that results contain `mukey`
#' # with `duplicates = TRUE`
#' x <- fetchSDA(sql, duplicates = TRUE)
#' 
#' # safely set texture class factor levels
#' # by making a copy of this column
#' # this will save in lieu of textures in the original
#' # `texture` column
#' horizons(x)$texture.class <- factor(x$texture, levels = SoilTextureLevels())
#' 
#' # graphical depiction of the result
#' plotSPC(x, color='texture.class', label='compname', 
#'         name='hzname', cex.names = 1, width=0.25, 
#'         plot.depth.axis=FALSE, hz.depths=TRUE, 
#'         name.style='center-center'
#' )
#'    
#'  }
#' }
SDA_spatialQuery <- function(geom, what='mukey', geomIntersection=FALSE) {
  
  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # sanity checks
  if(! what %in% c('mukey', 'geom')) {
    stop("query type must be either 'mukey' or 'geom'",call. = FALSE)
  }
  
  # geom must be an sp object
  if(! inherits(geom, 'Spatial')) {
    stop('`geom` must be a Spatial* object', call. = FALSE)
  }
  
  # geom must have a valid CRS
  if(is.na(proj4string(geom))) {
    stop('`geom` must have a valid CRS', call. = FALSE)
  }
  
  # CRS conversion if needed
  target.prj <- "+proj=longlat +datum=WGS84"
  if(proj4string(geom) != target.prj) {
    geom <- spTransform(geom, CRS(target.prj))
  }
  
  # WKT encoding
  # use a geometry collection
  wkt <- rgeos::writeWKT(geom, byid = FALSE)
  
  # slower query, returning geom + mukey
  # replacement for depreciated SDA_query_features()
  # 10-30x faster than spatial-returning query by input feature
  # TODO: this is 15x slower than non-spatial-returning-query in SDA_query_features()
  if(what == 'geom') {
    
    # return intersection
    if(geomIntersection) {
      q <- sprintf("
               SELECT 
                 mupolygongeo.STIntersection( geometry::STGeomFromText('%s', 4326) ).STAsText() AS geom, P.mukey
                 FROM mupolygon AS P
                 WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;", wkt, wkt)
    } else {
      # return overlapping
      q <- sprintf("
               SELECT 
                 mupolygongeo.STAsText() AS geom, P.mukey
                 FROM mupolygon AS P
                 WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;", wkt)
    }
    
    
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
  target.prj <- "+proj=longlat +datum=WGS84"
  if(proj4string(x) != target.prj) {
    geom <- spTransform(x, CRS(target.prj))
  }
  
  # iterate over features and save to list
  l <- list()
  n <- length(geom)
  # setup a progress bar for timing
  pb <- txtProgressBar(max=n, style=3)
  for(i in 1:n) {
    # make query
    res <- SDA_make_spatial_query(geom[i, ])
    # save results along with an ID
    res <- cbind(id=geom[[id]][i], res, stringsAsFactors = FALSE)
    names(res) <- c(id, 'mukey', 'muname')
    l[[i]] <- res
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # convert to data.frame, there may be > 1 row / feature when using lines / polygons
  d <- ldply(l)
  return(d)
}

