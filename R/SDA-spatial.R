#' Post-process WKT returned from SDA.
#' 
#' This is a helper function, commonly used with \code{SDA_query} to extract
#' WKT (well-known text) representation of geometry to an sp-class object.
#' 
#' The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}.
#' See the [SDA Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html) for detailed examples.
#' 
#' @param d \code{data.frame} returned by \code{SDA_query}, containing WKT
#' representation of geometry
#' @param g name of column in \code{d} containing WKT geometry
#' @param p4s PROJ4 CRS definition, typically GCS WGS84
#' 
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}. See the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for detailed examples.
#' 
#' @note This function requires the `httr`, `jsonlite`, `XML` ,  and `sf` packages.
#' 
#' @author D.E. Beaudette
#'
#' @return A \code{Spatial*} object.
#' @note This function requires the \code{httr}, \code{jsonlite}, \code{XML},
#' and \code{rgeos} packages.
#' @author D.E. Beaudette
#' @export processSDA_WKT
processSDA_WKT <- function(d, g='geom', p4s='+proj=longlat +datum=WGS84') {
  
  # SDA is always this CRS; proj4string approach obsolete
  stopifnot(g == 'geom', p4s == '+proj=longlat +datum=WGS84')
  
  # convert wkt to SPDF
  d[[g]] <- sf::st_as_sfc(wk::as_wkt(d[,g]))
  sfobj <- sf::st_as_sf(d)
  sfobj <- sf::st_set_crs(sfobj, sf::st_crs(4326))
  return(sf::as_Spatial(sfobj))
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
    },
    SAPOLYGON = { switch(method,
                        intersection = "
  WITH geom_data (geom, mukey) AS (
  SELECT 
  sapolygongeo.STIntersection( geometry::STGeomFromText('%s', 4326) ) AS geom, areasymbol
  FROM sapolygon
  WHERE sapolygongeo.STIntersects( geometry::STGeomFromText('%s', 4326) ) = 1
)
SELECT 
geom.STAsText() AS geom, mukey,
GEOGRAPHY::STGeomFromWKB(geom.STUnion(geom.STStartPoint()).STAsBinary(), 4326).STArea() * 0.000247105 AS area_ac
FROM geom_data;
  ",
                        
                        overlap = "SELECT 
                                     sapolygongeo.STAsText() AS geom, areasymbol
                                     FROM sapolygon
                                     WHERE sapolygongeo.STIntersects(geometry::STGeomFromText('%s', 4326) ) = 1")}
  )
  
  return(res)

}

#' Query Soil Data Access by spatial intersection with supplied geometry
#' 
#' Query SDA (SSURGO / STATSGO) records via spatial intersection with supplied
#' geometries. Input can be SpatialPoints, SpatialLines, or SpatialPolygons
#' objects with a valid CRS. Map unit keys, overlapping polygons, or the
#' spatial intersection of \code{geom} + SSURGO / STATSGO polygons can be
#' returned. See details.
#' 
#' Queries for map unit keys are always more efficient vs. queries for
#' overlapping or intersecting (i.e. least efficient) features. \code{geom} is
#' converted to GCS / WGS84 as needed. Map unit keys are always returned when
#' using \code{what = "mupolygon"}.
#' 
#' SSURGO (detailed soil survey, typically 1:24,000 scale) and STATSGO
#' (generalized soil survey, 1:250,000 scale) data are stored together within
#' SDA. This means that queries that don't specify an area symbol may result in
#' a mixture of SSURGO and STATSGO records. See the examples below and the
#' [SDA Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html)
#' for details.
#' 
#' @aliases SDA_spatialQuery SDA_make_spatial_query SDA_query_features
#' @param geom a Spatial* object, with valid CRS. May contain multiple
#' features.
#' @param what a character vector specifying what to return. `'mukey'`:
#' \code{data.frame} with intersecting map unit keys and names, `'mupolygon'`
#' overlapping or intersecting map unit polygons from selected database, `'areasymbol'`:
#' \code{data.frame} with intersecting soil survey areas, `'sapolygon'`:
#' overlapping or intersecting soil survey area polygons (SSURGO only) 
#' @param geomIntersection logical; \code{FALSE}: overlapping map unit polygons
#' returned, \code{TRUE}: intersection of \code{geom} + map unit polygons is
#' returned.
#' @param db a character vector identifying the Soil Geographic Databases
#' ('SSURGO' or 'STATSGO') to query. Option \var{STATSGO} currently works only
#' in combination with \code{what = "mupolygon"}. 
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @return A \code{data.frame} if \code{what = 'mukey'}, otherwise
#' \code{SpatialPolygonsDataFrame} object.
#' @note Row-order is not preserved across features in \code{geom} and returned
#' object. Use \code{sp::over()} or similar functionality to extract from
#' results. Polygon area in acres is computed server-side when \code{what =
#' 'geom'} and \code{geomIntersection = TRUE}.
#' @author D.E. Beaudette, A.G. Brown, D.R. Schlaepfer
#' @seealso \code{\link{SDA_query}}
#' @keywords manip
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet() & 
#'    requireNamespace("sp") &
#'    requireNamespace("scales") &
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
#' 
#' @export SDA_spatialQuery
SDA_spatialQuery <- function(geom,
                             what = 'mukey',
                             geomIntersection = FALSE,
                             db = c("SSURGO", "STATSGO", "SAPOLYGON"),
                             query_string = FALSE) {
  what <- tolower(what)
  db <- toupper(db)
  
  # sf support
  return_sf <- FALSE
  if (inherits(geom, 'sf') || inherits(geom, 'sfc')) {
    if (requireNamespace("sf")) {
      geom <- sf::as_Spatial(geom)
      return_sf <- TRUE
    }
  }
  
  # backwards compatibility with old value of what argument 
  if (what == 'geom') {
    message("converting what='geom' to what='mupolygon'")
    what <- "mupolygon"
  }
  
  # check for required packages
  if (!requireNamespace('sf', quietly = TRUE))
    stop('please install the `sf` package', call.=FALSE)
  
  if (!requireNamespace('wk', quietly = TRUE))
    stop('please install the `wk` package', call.=FALSE)
  
  # sanity checks
  if (!what %in% c('mukey', 'mupolygon', 'areasymbol', 'sapolygon')) {
    stop("query type (argument `what`) must be either 'mukey' / 'areasymbol' (tabular result) OR 'mupolygon' / 'sapolygon' (geometry result)", call. = FALSE)
  }

  # areasymbol is allowed with db = "SSURGO" (default) and db = "SAPOLYGON"
  if (what %in% c('areasymbol', 'sapolygon')) {
    db <- 'SAPOLYGON' # geometry selector uses db argument to specify sapolygon queries
  }
  
  db <- match.arg(db)
  
  if (what == "mukey" && db == "STATSGO") {
    stop("query type 'mukey' for 'STATSGO' is not supported", call. = FALSE)
  }
  
  if (what == 'areasymbol' && db == 'STATSGO') {
    stop("query type 'areasymbol' for 'STATSGO' is not supported", call. = FALSE)
  }
  
  # geom must be an sp object
  if(! inherits(geom, 'Spatial')) {
    stop('`geom` must be a Spatial* object', call. = FALSE)
  }
  
  # geom must have a valid CRS
  if (is.na(suppressWarnings(proj4string(geom)))) {
    stop('`geom` must have a valid CRS', call. = FALSE)
  }
  
  # CRS conversion if needed
  target.prj <- "+proj=longlat +datum=WGS84"
  if (suppressWarnings(proj4string(geom)) != target.prj) {
    geom <- spTransform(geom, CRS(target.prj))
  }
  
  # WKT encoding
  # use a geometry collection
  wkt <- wk::wk_collection(wk::as_wkt(sf::st_as_sf(geom)))
  
  # returning geom + mukey or geom + areasymbol
  if (what %in% c('mupolygon', 'sapolygon')) {

    # return intersection + area
    if (geomIntersection) {
      
      # select the appropriate query
      .template <- .SDA_geometrySelector(db = db, method = 'intersection')
      q <- sprintf(.template, as.character(wkt), as.character(wkt))
                   
    } else {
      # return overlapping
      
      # select the appropriate query
      .template <- .SDA_geometrySelector(db = db, method = 'overlap')
      q <- sprintf(.template, as.character(wkt))
    }
    
    if (query_string) {
      return(q)
    }
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
    res <- processSDA_WKT(res)
  }
  
  # SSURGO only
  if (what == 'mukey') {
    q <- sprintf("SELECT mukey, muname
                FROM mapunit
                WHERE mukey IN (
                SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('%s')
                )", wkt)
    
    if (query_string) {
      return(q)
    }
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
  }
  
  # SSURGO only
  if (what == 'areasymbol') {
    q <- sprintf("SELECT areasymbol
                FROM sapolygon
                WHERE sapolygonkey IN (
                SELECT DISTINCT sapolygonkey from SDA_Get_Sapolygonkey_from_intersection_with_WktWgs84('%s')
                )", wkt)
    
    if (query_string) {
      return(q)
    }
    
    # single query for all of the features
    # note that row-order / number of rows in results may not match geom
    res <- suppressMessages(SDA_query(q))
  }
  
  if (inherits(res, 'Spatial') && return_sf) {
    res <- sf::st_as_sf(res)
  }
  
  return(res)
}
