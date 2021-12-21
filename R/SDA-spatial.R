#' Post-process Well-Known Text from Soil Data Access
#' 
#' This is a helper function commonly used with \code{SDA_query} to extract
#' WKT (well-known text) representation of geometry to an `sf` or `sp` object.
#' 
#' The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}.
#' See the [SDA Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html) for detailed examples.
#' 
#' @param d \code{data.frame} returned by \code{SDA_query}, containing WKT representation of geometry
#' @param g name of column in `d` containing WKT geometry
#' @param crs CRS definition (e.g. an EPSG code). Default `4326` for WGS84 Geographic Coordinate System
#' @param p4s Deprecated: PROJ4 CRS definition
#' @param as_sf Return an `sf` `data.frame`? If `FALSE` return a `Spatial*` object.
#' @details The SDA website can be found at \url{https://sdmdataaccess.nrcs.usda.gov}. See the \href{http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html}{SDA Tutorial} for detailed examples.
#' 
#' @note This function requires the `sf` package.
#' 
#' @author D.E. Beaudette, A.G. Brown
#'
#' @return An `sf` object or if `as_sf` is `FALSE` a `Spatial*` object.
#' @author D.E. Beaudette
#' @export processSDA_WKT
processSDA_WKT <- function(d, g='geom', crs = 4326, p4s = NULL, as_sf = TRUE) {
  if (!is.null(p4s)) {
    .Deprecated(msg = "Passing PROJ4 strings via `p4s` is deprecated. SDA interfaces in soilDB use the WGS84 Geographic Coordinate System (EPSG:4326) by default. Use the `crs` argument to customize.")
  }
  
  if (!requireNamespace("sf")) {
    stop("package `sf` is required", call. = FALSE)
  }
  
  # convert wkt to sf/SPDF
  d[[g]] <- sf::st_as_sfc(wk::as_wkt(d[,g]))
  sfobj <- sf::st_as_sf(d)
  sfobj <- sf::st_set_crs(sfobj, sf::st_crs(crs))
  if (as_sf) {
    return(sfobj)
  }
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
#' Query SDA (SSURGO / STATSGO) records via spatial intersection with supplied geometries. Input can be SpatialPoints, SpatialLines, or SpatialPolygons objects with a valid CRS. Map unit keys, overlapping polygons, or the spatial intersection of \code{geom} + SSURGO / STATSGO polygons can be returned. See details.
#' 
#' Queries for map unit keys are always more efficient vs. queries for overlapping or intersecting (i.e. least efficient) features. \code{geom} is converted to GCS / WGS84 as needed. Map unit keys are always returned when using \code{what = "mupolygon"}.
#' 
#' SSURGO (detailed soil survey, typically 1:24,000 scale) and STATSGO (generalized soil survey, 1:250,000 scale) data are stored together within SDA. This means that queries that don't specify an area symbol may result in a mixture of SSURGO and STATSGO records. See the examples below and the [SDA Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html) for details.
#' 
#' @aliases SDA_spatialQuery
#' @param geom an `sf` or `Spatial*` object, with valid CRS. May contain multiple features.
#' @param what a character vector specifying what to return. `'mukey'`: `data.frame` with intersecting map unit keys and names, `'mupolygon'` overlapping or intersecting map unit polygons from selected database, `'areasymbol'`: `data.frame` with intersecting soil survey areas, `'sapolygon'`: overlapping or intersecting soil survey area polygons (SSURGO only) 
#' @param geomIntersection logical; `FALSE`: overlapping map unit polygons returned, `TRUE`: intersection of `geom` + map unit polygons is returned.
#' @param db a character vector identifying the Soil Geographic Databases (`'SSURGO'` or `'STATSGO'`) to query. Option \var{STATSGO} works with `what = "mukey"` and `what = "mupolygon"`. 
#' @param byFeature Iterate over features, returning a combined data.frame where each feature is uniquely identified by value in `idcol`. Default `FALSE`.
#' @param idcol Unique IDs used for individual features when `byFeature = TRUE`; Default `"gid"`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @return A `data.frame` if `what = 'mukey'`, otherwise a `SpatialPolygonsDataFrame` or `sf` object.
#' @note Row-order is not preserved across features in \code{geom} and returned object. Use `byFeature` argument to iterate over features and return results that are 1:1 with the inputs. Polygon area in acres is computed server-side when `what = 'mupolygon'` and `geomIntersection = TRUE`.
#' @author D.E. Beaudette, A.G. Brown, D.R. Schlaepfer
#' @seealso \code{\link{SDA_query}}
#' @keywords manip
#' @examples
#' \donttest{
#'   if (requireNamespace("aqp") && requireNamespace("sf")) {
#'     
#'     library(aqp)
#'     library(sf)
#'     
#'     ## query at a point
#'     
#'     # example point
#'     p <- sf::st_as_sf(data.frame(x = -119.72330, 
#'                                  y = 36.92204),
#'                       coords = c('x', 'y'),
#'                       crs = 4326)
#'     
#'     # query map unit records at this point
#'     res <- SDA_spatialQuery(p, what = 'mukey')
#'     
#'     # convert results into an SQL "IN" statement
#'     # useful when there are multiple intersecting records
#'     mu.is <- format_SQL_in_statement(res$mukey)
#'     
#'     # composite SQL WHERE clause
#'     sql <- sprintf("mukey IN %s", mu.is)
#'     
#'     # get commonly used map unit / component / chorizon records
#'     # as a SoilProfileCollection object
#'     # request that results contain `mukey` with `duplicates = TRUE`
#'     x <- fetchSDA(sql, duplicates = TRUE)
#'     
#'     # safely set texture class factor levels
#'     # by making a copy of this column
#'     # this will save in lieu of textures in the original
#'     # `texture` column
#'     horizons(x)$texture.class <- factor(x$texture, levels = SoilTextureLevels())
#'     
#'     # graphical depiction of the result
#'     plotSPC(x,
#'             color = 'texture.class',
#'             label = 'compname',
#'             name = 'hzname',
#'             cex.names = 1,
#'             width = 0.25,
#'             plot.depth.axis = FALSE,
#'             hz.depths = TRUE,
#'             name.style = 'center-center')
#'     
#'     ## query mukey + geometry that intersect with a bounding box
#'     
#'     # define a bounding box: xmin, xmax, ymin, ymax
#'     #
#'     #         +-------------------(ymax, xmax)
#'     #         |                        |
#'     #         |                        |
#'     #     (ymin, xmin) ----------------+
#'     b <- c(-119.747629, -119.67935, 36.912019, 36.944987)
#'     
#'     # convert bounding box to WKT
#'     bbox.sp <- sf::st_as_sf(wk::rct(
#'       xmin = b[1], xmax = b[2], ymin = b[3], ymax = b[4],
#'       crs = sf::st_crs(4326)
#'     ))
#'     
#'     # results contain associated map unit keys (mukey)
#'     # return SSURGO polygons, after intersection with provided BBOX
#'     ssurgo.geom <- SDA_spatialQuery(
#'       bbox.sp,
#'       what = 'mupolygon',
#'       db = 'SSURGO',
#'       geomIntersection = TRUE
#'     )
#'     
#'     # return STATSGO polygons, after intersection with provided BBOX
#'     statsgo.geom <- SDA_spatialQuery(
#'       bbox.sp,
#'       what = 'mupolygon',
#'       db = 'STATSGO',
#'       geomIntersection = TRUE
#'     )
#'     
#'     # inspect results
#'     par(mar = c(0,0,3,1))
#'     plot(sf::st_geometry(ssurgo.geom), border = 'royalblue')
#'     plot(sf::st_geometry(statsgo.geom), lwd = 2, border = 'firebrick', add = TRUE)
#'     plot(sf::st_geometry(bbox.sp), lwd = 3, add = TRUE)
#'     legend(
#'       x = 'topright',
#'       legend = c('BBOX', 'STATSGO', 'SSURGO'),
#'       lwd = c(3, 2, 1),
#'       col = c('black', 'firebrick', 'royalblue'),
#'     )
#'     
#'     # quick reminder that STATSGO map units often contain many components
#'     # format an SQL IN statement using the first STATSGO mukey
#'     mu.is <- format_SQL_in_statement(statsgo.geom$mukey[1])
#'     
#'     # composite SQL WHERE clause
#'     sql <- sprintf("mukey IN %s", mu.is)
#'     
#'     # get commonly used map unit / component / chorizon records
#'     # as a SoilProfileCollection object
#'     x <- fetchSDA(sql)
#'     
#'     # tighter figure margins
#'     par(mar = c(0,0,3,1))
#'     
#'     # organize component sketches by national map unit symbol
#'     # color horizons via awc
#'     # adjust legend title
#'     # add alternate label (vertical text) containing component percent
#'     # move horizon names into the profile sketches
#'     # make profiles wider
#'     aqp::groupedProfilePlot(x,
#'                             groups = 'nationalmusym',
#'                             label = 'compname',
#'                             color = 'awc_r',
#'                             col.label = 'Available Water Holding Capacity (cm / cm)',
#'                             alt.label = 'comppct_r',
#'                             name.style = 'center-center',
#'                             width = 0.3
#'     )
#'     
#'     mtext(
#'       'STATSGO (1:250,000) map units contain a lot of components!',
#'       side = 1,
#'       adj = 0,
#'       line = -1.5,
#'       at = 0.25,
#'       font = 4
#'     )
#'   }
#' }
#' @export SDA_spatialQuery
SDA_spatialQuery <- function(geom,
                             what = 'mukey',
                             geomIntersection = FALSE,
                             db = c("SSURGO", "STATSGO", "SAPOLYGON"),
                             byFeature = FALSE,
                             idcol = "gid",
                             query_string = FALSE) {
  if (byFeature) {
    res <- do.call('rbind', lapply(1:nrow(geom), function(i) {
      res2 <- .SDA_spatialQuery(
        geom = geom[i, ],
        what = what,
        geomIntersection = geomIntersection,
        db = db,
        query_string = query_string
      )
      res2[[idcol]] <- i
      res2
    }))
    return(res)
  }
  .SDA_spatialQuery(
    geom = geom,
    what = what,
    geomIntersection = geomIntersection,
    db = db,
    query_string = query_string
  )
}
.SDA_spatialQuery <- function(geom,
                             what = 'mukey',
                             geomIntersection = FALSE,
                             db = c("SSURGO", "STATSGO", "SAPOLYGON"),
                             query_string = FALSE) {
  # check for required packages
  if (!requireNamespace('sf', quietly = TRUE))
    stop('please install the `sf` package', call.=FALSE)
  
  if (!requireNamespace('wk', quietly = TRUE))
    stop('please install the `wk` package', call.=FALSE)
  
  what <- tolower(what)
  db <- toupper(db)
  
  # sp support
  return_sf <- FALSE
  if (inherits(geom, 'sf') || inherits(geom, 'sfc')) {
    return_sf <- TRUE
  } else if (inherits(geom, 'Spatial')) {
    geom <- sf::st_as_sf(geom)
  } else {
    stop('`geom` must be an sf or Spatial* object', call. = FALSE)
  }
  
  # backwards compatibility with old value of what argument 
  if (what == 'geom') {
    message("converting what='geom' to what='mupolygon'")
    what <- "mupolygon"
  }
  
  # sanity checks
  if (!what %in% c('mukey', 'mupolygon', 'areasymbol', 'sapolygon')) {
    stop("query type (argument `what`) must be either 'mukey' / 'areasymbol' (tabular result) OR 'mupolygon' / 'sapolygon' (geometry result)", call. = FALSE)
  }

  # areasymbol is allowed with db = "SSURGO" (default) and db = "SAPOLYGON"
  if (what %in% c('areasymbol', 'sapolygon')) {
    db <- 'SAPOLYGON' # geometry selector uses db argument to specify sapolygon queries
  }
  
  db <- match.arg(db)
  
  if (what == 'areasymbol' && db == 'STATSGO') {
    stop("query type 'areasymbol' for 'STATSGO' is not supported", call. = FALSE)
  }
  
  # geom must have a valid CRS
  if (is.na(sf::st_crs(geom)$wkt)) {
    stop('`geom` must have a valid CRS', call. = FALSE)
  }
  
  # CRS conversion if needed
  target.prj <- sf::st_crs(4326)
  if (suppressWarnings(sf::st_crs(geom)) != target.prj) {
    geom <- sf::st_transform(geom, target.prj)
  }
  
  # WKT encoding
  # use a geometry collection
  wkt <- wk::wk_collection(wk::as_wkt(geom))
  
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
    res <- processSDA_WKT(res, as_sf = return_sf)
  }
  
  if (what == 'mukey') {
    if (db == "SSURGO") {
      q <- sprintf("SELECT mukey, muname
                  FROM mapunit
                  WHERE mukey IN (
                  SELECT DISTINCT mukey from SDA_Get_Mukey_from_intersection_with_WktWgs84('%s')
                  )", wkt)
    } else if (db == "STATSGO") {
      q <- sprintf("SELECT DISTINCT P.mukey, mapunit.muname
                    FROM gsmmupolygon AS P
                    INNER JOIN mapunit ON mapunit.mukey = P.mukey
                    WHERE mupolygongeo.STIntersects(geometry::STGeomFromText('%s', 4326) ) = 1 
                      AND CLIPAREASYMBOL = 'US'", wkt)
    } else {
      stop("query type 'mukey' for 'SAPOLYGON' is not supported", call. = FALSE)
    }
    
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
  
  return(res)
}
