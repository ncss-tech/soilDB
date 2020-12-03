
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

#' @title Post-process WKT returned from SDA.
#' 
#' @description This is a helper function, commonly used with \code{SDA_query} to extract WKT (well-known text) representation of geometry to an sp-class object.
#' 
#' @param d \code{data.frame} returned by \code{SDA_query}, containing WKT representation of geometry
#' @param g name of column in \code{d} containing WKT geometry
#' @param p4s PROJ4 CRS definition, typically GCS WGS84
#' 
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}. See the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for detailed examples.
#' 
#' @note This function requires the `httr`, `jsonlite`, `XML`, and `rgeos` packages.
#' 
#' @author D.E. Beaudette
#' 
#' @return A \code{Spatial*} object.
#' 
processSDA_WKT <- function(d, g='geom', p4s='+proj=longlat +datum=WGS84') {
  # iterate over features (rows) and convert into list of SPDF
  p <- list()
  n <- nrow(d)
  
  # points or polygons?
  # looking at the first feature for efficiency, all others should be the same
  g.type <- class(rgeos::readWKT(d[1, g]))
  
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
  }
  
  # reduce list to single SPDF
  spdf <- do.call('rbind', p)
  
  return(spdf)
}



## TODO consider adding an 'identity' method for more generic use
# select the right query for SSURGO / STATSGO geometry filters submitted to SDA
# this is important because STATSGO features require the additional
# AND CLIPAREASYMBOL = 'US'
.SDA_geometrySelector <- function(db, method) {
 
  res <- switch(db,
    SSURGO = {
      switch(method,
             intersection = {
               "
  WITH geom_data (geom, mukey) AS (
  SELECT 
  mupolygongeo.STIntersection( geometry::STGeomFromText('%s', 4326) ) AS geom, P.mukey
  FROM mupolygon AS P
  WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1
)
SELECT 
geom.STAsText() AS geom, mukey,
GEOGRAPHY::STGeomFromWKB(
    geom.STUnion(geom.STStartPoint()).STAsBinary(), 4326).STArea() * 0.000247105 AS area_ac
FROM geom_data;
  "
             },
             
             overlap = {
               "
  SELECT 
  mupolygongeo.STAsText() AS geom, P.mukey
  FROM mupolygon AS P
  WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1;
"
             }
             )
    },
    
    STATSGO = {
      switch(method,
             intersection = {
               "
  WITH geom_data (geom, mukey) AS (
  SELECT 
  mupolygongeo.STIntersection( geometry::STGeomFromText('%s', 4326) ) AS geom, P.mukey
  FROM gsmmupolygon AS P
  WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1
  AND CLIPAREASYMBOL = 'US'
)
SELECT 
geom.STAsText() AS geom, mukey,
GEOGRAPHY::STGeomFromWKB(
    geom.STUnion(geom.STStartPoint()).STAsBinary(), 4326).STArea() * 0.000247105 AS area_ac
FROM geom_data;
  "
             },
             
             overlap = {
               "
  SELECT 
  mupolygongeo.STAsText() AS geom, P.mukey
  FROM gsmmupolygon AS P
  WHERE mupolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1
  AND CLIPAREASYMBOL = 'US' ;
"
               
             }
      )
    }
  )
  
  return(res)

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
#' @description Query SDA (SSURGO / STATSGO) records via spatial intersection with supplied geometries. Input can be SpatialPoints, SpatialLines, or SpatialPolygons objects with a valid CRS. Map unit keys, overlapping polygons, or the spatial intersection of \code{geom} + SSURGO / STATSGO polygons can be returned. See details.
#' 
#' @param geom a Spatial* object, with valid CRS. May contain multiple features.
#' @param what a character vector specifying what to return. 'mukey': \code{data.frame} with intersecting map unit keys and names, \code{geom} overlapping or intersecting map unit polygons
#' @param geomIntersection logical; \code{FALSE}: overlapping map unit polygons returned, \code{TRUE}: intersection of \code{geom} + map unit polygons is returned.
#' @param db a character vector identifying the Soil Geographic Databases
#'   ('SSURGO' or 'STATSGO') to query. Option \var{STATSGO} currently works
#'   only in combination with \code{what = "geom"}.
#'
#' @return A \code{data.frame} if \code{what = 'mukey'}, otherwise \code{SpatialPolygonsDataFrame} object.
#' 
#' @author D.E. Beaudette, A.G. Brown, D.R. Schlaepfer
#' @seealso \code{\link{SDA_query}}
#' @keywords manip
#' 
#' @aliases SDA_make_spatial_query SDA_query_features
#' 
#' @note Row-order is not preserved across features in \code{geom} and returned object. Use \code{sp::over()} or similar functionality to extract from results. Polygon area in acres is computed server-side when \code{what = 'geom'} and \code{geomIntersection = TRUE}.
#' 
#' 
#' @details Queries for map unit keys are always more efficient vs. queries for overlapping or intersecting (i.e. least efficient) features. \code{geom} is converted to GCS / WGS84 as needed. Map unit keys are always returned when using \code{what = "geom"}.
#' 
#' There is a 100,000 record limit and 32Mb JSON serializer limit, per query.
#' 
#' SSURGO (detailed soil survey, typically 1:24,000 scale) and STATSGO (generalized soil survey, 1:250,000 scale) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for details.
#' 
#' 
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() & 
#'    requireNamespace("sp") &
#'    requireNamespace("raster") 
#'    ) {
#' 
#' library(aqp)
#' library(sp)
#' library(raster)
#' 
#' ## query at a point
#' 
#' # example point
#' p <- SpatialPoints(cbind(x = -119.72330, y = 36.92204), 
#'                    proj4string = CRS('+proj=longlat +datum=WGS84'))
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
#' 
#' 
#' ## query mukey + geometry that intersect with a bounding box
#' 
#' # define a bounding box: xmin, xmax, ymin, ymax
#' #
#' #         +-------------------(ymax, xmax)
#' #         |                        |
#' #         |                        |
#' #     (ymin, xmin) ----------------+
#' b <- c(-119.747629, -119.67935, 36.912019, 36.944987)
#' 
#' # convert bounding box to WKT
#' bbox.sp <-as(extent(b), 'SpatialPolygons')
#' proj4string(bbox.sp) <- '+proj=longlat +datum=WGS84'
#' 
#' # results contain associated map unit keys (mukey)
#' # return SSURGO polygons, after intersection with provided BBOX
#' ssurgo.geom <- SDA_spatialQuery(
#'   bbox.sp, 
#'   what = 'geom', 
#'   db = 'SSURGO', 
#'   geomIntersection = TRUE
#' )
#' 
#' # return STATSGO polygons, after intersection with provided BBOX
#' statsgo.geom <- SDA_spatialQuery(
#'   bbox.sp, 
#'   what = 'geom', 
#'   db = 'STATSGO', 
#'   geomIntersection = TRUE
#' )
#' 
#' # inspect results
#' par(mar = c(0,0,3,1))
#' plot(ssurgo.geom, border = 'royalblue')
#' plot(statsgo.geom, lwd = 2, border = 'firebrick', add = TRUE)
#' plot(bbox.sp, lwd = 3, add = TRUE)
#' legend(
#'   x = 'topright', 
#'   legend = c('BBOX', 'STATSGO', 'SSURGO'), 
#'   lwd = c(3, 2, 1),
#'   col = c('black', 'firebrick', 'royalblue'),
#' )
#' 
#' 
#' # quick reminder that STATSGO map units often contain many components
#' # format an SQL IN statement using the first STATSGO mukey
#' mu.is <- format_SQL_in_statement(statsgo.geom$mukey[1])
#' 
#' # composite SQL WHERE clause
#' sql <- sprintf("mukey IN %s", mu.is)
#' 
#' # get commonly used map unit / component / chorizon records
#' # as a SoilProfileCollection object
#' x <- fetchSDA(sql)
#' 
#' # tighter figure margins
#' par(mar = c(0,0,3,1))
#' 
#' 
#' # organize component sketches by national map unit symbol
#' # color horizons via awc
#' # adjust legend title
#' # add alternate label (vertical text) containing component percent
#' # move horizon names into the profile sketches
#' # make profiles wider
#' groupedProfilePlot(
#'   x, 
#'   groups = 'nationalmusym', 
#'   label = 'compname', 
#'   color = 'awc_r', 
#'   col.label = 'Available Water Holding Capacity (cm / cm)',
#'   alt.label = 'comppct_r',
#'   name.style = 'center-center',
#'   width = 0.3
#' )
#' 
#' 
#' mtext(
#'   'STATSGO (1:250,000) map units contain a lot of components!', 
#'   side = 1, 
#'   adj = 0, 
#'   line = -1.5, 
#'   at = 0.25, 
#'   font = 4
#' )
#'  }
#' }
#' 
SDA_spatialQuery <- function(geom, what='mukey', geomIntersection=FALSE,
  db = c("SSURGO", "STATSGO")) {

  # check for required packages
  if(!requireNamespace('rgeos', quietly = TRUE))
    stop('please install the `rgeos` package', call.=FALSE)
  
  # sanity checks
  if(! what %in% c('mukey', 'geom')) {
    stop("query type must be either 'mukey' or 'geom'",call. = FALSE)
  }

  db <- match.arg(db)

  if (what == "mukey" && db == "STATSGO") {
    stop("query type 'mukey' for 'STATSGO' is not supported", call. = FALSE)
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

    # return intersection + area
    if(geomIntersection) {
      
      # select the appropriate query
      .template <- .SDA_geometrySelector(db = db, method = 'intersection')
      q <- sprintf(.template, wkt, wkt)
                   
    } else {
      # return overlapping
      
      # select the appropriate query
      .template <- .SDA_geometrySelector(db = db, method = 'overlap')
      q <- sprintf(.template, wkt)
    }
    
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
    res <- processSDA_WKT(res)
  }
  
  # SSURGO only
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


## now deprecated
SDA_make_spatial_query <- function(i) {
  
  .Deprecated(new = 'SDA_spatialQuery')
  
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
## now deprecated
SDA_query_features <- function(x, id='pedon_id') {
  
  .Deprecated(new = 'SDA_spatialQuery')
  
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

