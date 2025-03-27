#' Get Spatial Data from Soil Data Access by `mukey`, `nationalmusym` or `areasymbol`
#'
#' @description This method facilitates queries to Soil Data Access (SDA) mapunit and survey area geometry. Queries are generated based on map unit key (`mukey`) and national map unit symbol (`nationalmusym`) for `mupolygon` (SSURGO) or `gsmmupolygon` (STATSGO) geometry OR legend key (`lkey`) and area symbols (`areasymbol`) for `sapolygon` (Soil Survey Area; SSA) geometry).
#'
#' A Soil Data Access query returns geometry and key identifying information about the map unit or area of interest. Additional columns from the map unit or legend table can be included; see `add.fields` argument.
#'
#' @param x A vector of map unit keys (`mukey`) or national map unit symbols (`nationalmusym`) for `mupolygon`, `muline` or `mupoint`; feature keys (`featkey`) for `featpoint` and `featline`; legend keys (`lkey`) or soil survey area symbols (`areasymbol`) for `sapolygon` geometry. If `geom.src="mlrapolygon"` then `x` refers to `MLRARSYM` (major land resource area symbols).
#' @param by.col Column name containing map unit identifier `"mukey"`, `"nationalmusym"`, or `"ecoclassid"` for `geom.src` `mupolygon` OR `"areasymbol"`, `"areaname"`, `"mlraoffice"`, `"mouagncyresp"` for `geom.src` `sapolygon`; default is determined by `isTRUE(is.numeric(x))` for `mukey`, `featkey` or `lkey`, using `nationalmusym` or `areasymbol` otherwise.
#' @param method geometry result type: `"feature"` returns polygons, `"bbox"` returns the bounding box of each polygon (via `STEnvelope()`), `"point"` returns a single point (via `STPointOnSurface()`) within each polygon, `"extent"` returns an aggregate bounding box (the extent of all polygons, `geometry::EnvelopeAggregate()`) ), `"convexhull"` (`geometry::ConvexHullAggregate()`) returns the aggregate convex hull around all polygons, `"union"` (`geometry::UnionAggregate()`) and `"collection"` (`geometry::CollectionAggregate()`) return a `MULTIPOLYGON` or a `GEOMETRYCOLLECTION`, respectively, for each `mukey`, `nationalmusym`, or `areasymbol `. In the case of the latter four aggregation methods, the groups for aggregation depend on `by.col` (default by `"mukey"`).
#' @param geom.src Either `mupolygon` (map unit polygons), `muline` (map unit lines), `mupoint` (map unit points), `featpoint` (feature points), `featline` (feature lines), `sapolygon` (soil survey area boundary polygons), or `mlrapolygon` (major land resource area boundary polygons)
#' @param db Default: `"SSURGO"`. When `geom.src` is `mupolygon`, use STATSGO polygon geometry instead of SSURGO by setting `db = "STATSGO"`
#' @param add.fields Column names from `mapunit` or `legend` table to add to result. Must specify parent table name as the prefix before column name e.g. `mapunit.muname`.
#' @param chunk.size Number of values of `x` to process per query. Necessary for large results. Default: `10`
#' @param verbose Print messages?
#' @param as_Spatial Return sp classes? e.g. `Spatial*DataFrame`. Default: `FALSE`.
#' @return an `sf` data.frame corresponding to SDA spatial data for all symbols requested. If `as_Spatial=TRUE` returns a `Spatial*DataFrame` from the sp package via `sf::as_Spatial()` for backward compatibility. Default result contains geometry with attribute table containing unique feature ID, symbol and area symbol plus additional fields in result specified with `add.fields`.
#' @details
#'
#' This function automatically "chunks" the input vector (using `makeChunks()`) of map unit identifiers to minimize the likelihood of exceeding the SDA data request size. The number of chunks varies with the `chunk.size` setting and the length of your input vector. If you are working with many map units and/or large extents, you may need to decrease this number in order to have more chunks.
#'
#' Querying regions with complex mapping may require smaller `chunk.size`. Numerically adjacent IDs in the input vector may share common qualities (say, all from same soil survey area or region) which could cause specific chunks to perform "poorly" (slow or error) no matter what the chunk size is. Shuffling the order of the inputs using `sample()` may help to eliminate problems related to this, depending on how you obtained your set of MUKEY/nationalmusym to query. One could feasibly use `muacres` as a heuristic to adjust for total acreage within chunks.
#'
#' Note that STATSGO data are fetched where `CLIPAREASYMBOL = 'US'` to avoid duplicating state and national subsets of the geometry.
#' 
#' A prototype interface, `geom.src="mlrapolygon"`, is provided for obtaining Major Land Resource Area (MLRA) polygon 
#' boundaries. When using this geometry source `x` is a vector of `MLRARSYM` (MLRA Symbols). The geometry source is
#' the MLRA Geographic Database v5.2 (2022) which is not (yet) part of Soil Data Access. Instead of SDA, GDAL utilities
#' are used to read a zipped ESRI Shapefile from a remote URL: <https://www.nrcs.usda.gov/sites/default/files/2022-10/MLRA_52_2022.zip>.
#' Therefore, most additional `fetchSDA_spatial()` arguments are _not_ currently supported for the MLRA geometry source. 
#' In the future a `mlrapolygon` table may be added to SDA (analogous to  `mupolygon` and `sapolygon`), 
#' and the function will be updated accordingly at that time.
#' 
#' @author Andrew G. Brown, Dylan E. Beaudette
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE)
#' \donttest{
#'  
#'     # get spatial data for a single mukey
#'     single.mukey <- try(fetchSDA_spatial(x = "2924882"))
#'
#'     # demonstrate fetching full extent (multi-mukey) of national musym
#'     full.extent.nmusym <- try(fetchSDA_spatial(x = "2x8l5", by = "nmusym"))
#'
#'     # compare extent of nmusym to single mukey within it
#'     if (!inherits(single.mukey, 'try-error') && 
#'         !inherits(full.extent.nmusym, 'try-error')) {
#'         
#'         if (requireNamespace("sf")) {
#'       
#'          plot(sf::st_geometry(full.extent.nmusym), col = "RED", border = 0)
#'          plot(sf::st_geometry(single.mukey), add = TRUE, col = "BLUE", border = 0)
#'        
#'         }
#'         
#'     }
#'
#'     # demo adding a field (`muname`) to attribute table of result
#'     head(try(fetchSDA_spatial(x = "2x8l5", by="nmusym", add.fields="muname")))
#' }
#' @rdname fetchSDA_spatial
#' @export fetchSDA_spatial
fetchSDA_spatial <- function(x,
                             by.col = "mukey",
                             method = 'feature',
                             geom.src = 'mupolygon',
                             db = 'SSURGO',
                             add.fields = NULL,
                             chunk.size = 10,
                             verbose = TRUE,
                             as_Spatial = getOption('soilDB.return_Spatial', default = FALSE)) {
  by.col <- tolower(by.col)
  geom.src <- match.arg(tolower(geom.src), choices = c("mupolygon", "sapolygon", "mlrapolygon",
                                                       "mupoint", "muline", "featpoint", "featline"))
  db <- match.arg(toupper(db), choices = c("SSURGO", "STATSGO"))

  # survey area polygons, point, and line mapunits/features only available in SSURGO
  if (geom.src %in% c("sapolygon", "mupoint", "muline", "featpoint", "featline")) {
    db <- 'SSURGO'
  }
  
  # statsgo flag
  use_statsgo <- (db == "STATSGO")

  tstart <- Sys.time()

  method <- match.arg(tolower(method), c('feature', 'bbox', 'extent', 'envelope', 'point', 'convexhull', 'union', 'collection'))

  if (missing(x)) {
    x <- NULL
  }
  
  # remove any redundancy in input off the top -- this is important
  # in case x is not ordered and contains duplicates which will possibly
  # be in different chunks
  x <- unique(x)

  if (geom.src == "mlrapolygon") {
    # mlra polygons are not part of SSURGO or STATSGO
    if (by.col == "mukey") {
      by.col <- "MLRARSYM"
    }
  } else if (geom.src == 'sapolygon' && (by.col %in% c("mukey", "nmusym", "nationalmusym"))) {
    # lkey and areasymbol are the option for sapolygon
    if (is.numeric(x)) {
      by.col <- "lkey"
    } else {
      by.col <- "areasymbol"
    }
  }

  # default interface is mukey/lkey/featkey
  if (by.col %in% c("mukey", "lkey", "featkey")) {
    
    mukey.list <- x
    
  # a convenience interface is by nmusym -- may have several mukey per nmusym
  } else if (by.col == "nmusym" || by.col == "nationalmusym") {

    # do additional query to determine mapping of nmusym:mukey
    q.mukey <- paste0("SELECT nationalmusym, mukey FROM mapunit WHERE nationalmusym IN ",
                      format_SQL_in_statement(x),";")

    suppressMessages( {res <- SDA_query(q.mukey)} )

    if (inherits(res, 'try-error')) {
      message("fetchSDA_spatial: fatal error in national mapunit -> mukey conversion.", call. = FALSE)
      return(res)
    }
    mukey.list <- unique(res$mukey)
   } else if (by.col == "ecoclassid") {
      
      # do additional query to determine mapping of nmusym:mukey
      q.esid <- paste0("SELECT DISTINCT nationalmusym, mapunit.mukey FROM mapunit 
                        INNER JOIN component ON component.mukey = mapunit.mukey 
                        INNER JOIN coecoclass ON coecoclass.cokey = component.cokey
                        WHERE ecoclassid IN ",
                        format_SQL_in_statement(x),";")
      
      suppressMessages( {res <- SDA_query(q.esid)} )
      
      if (inherits(res, 'try-error')) {
        message("fetchSDA_spatial: fatal error in ecoclassid -> mukey conversion.")
        return(res)
      }
      mukey.list <- unique(res$mukey)
      
  # a convenience interface for lkey is by areasymbol/areaname or other legend column
  } else if (by.col %in% c("areasymbol", "areasym", "areaname", "mlraoffice", "mouagncyresp")) {
    if (by.col == "areasym") {
      by.col <- "areasymbol"
    }

    if (by.col != "areasymbol") {
      add.fields <- unique(c(add.fields, by.col))
    }
    
    # do additional query to determine mapping of areasymbol:lkey
    q.mukey <- paste0("SELECT areasymbol, lkey FROM legend WHERE ", by.col, " IN ",
                      format_SQL_in_statement(x),";")

    suppressMessages( {res <- SDA_query(q.mukey)} )

    if (inherits(res, 'try-error')) {
      message("fetchSDA_spatial: fatal error in ", by.col, " -> lkey conversion.", call. = FALSE)
      return(res)
    }
    
    mukey.list <- unique(res$lkey)
    
  } else if (geom.src == "mlrapolygon") {
    if (!requireNamespace("sf")) {
      stop("package 'sf' is required to read MLRA boundaries from ZIP file source", call. = FALSE)
    }
    res <- sf::read_sf(
      "/vsizip//vsicurl/https://s3-fpac-nrcs-dshub-public.s3.us-gov-west-1.amazonaws.com/MLRA_52_2022.zip",
      query = paste0(
        "SELECT * FROM MLRA_52 ",
        ifelse(
          is.null(x),
          "",
          sprintf("WHERE %s IN %s", by.col, format_SQL_in_statement(x))
        )),
      as_tibble = FALSE,
      stringsAsFactors = FALSE
    )
    # use "geom" for consistency with other spatial outputs from SDA; requires sf >= 1.0-6
    sf::st_geometry(res) <- "geom"
    # TODO: could provide custom MLRA aggregation methods here: centroid, bbox, convex hull?
    #       in the future a T-SQL implementation would allow for any of the defined method options
    return(res)
  } else {
    return(try(stop(paste0("Unknown feature identifier (", by.col, ")"), call. = FALSE)))
  }

  mukey.chunk <- makeChunks(mukey.list, chunk.size)

  s <- NULL

  # alias
  if (method == "envelope") {
    method <- "extent"
  }
  
  # select method
  geom.type <- switch(method,
                      feature = 'mupolygongeo.STAsText()',
                      bbox = 'mupolygongeo.STEnvelope().STAsText()',
                      collection = 'geometry::CollectionAggregate(mupolygongeo).STAsText()',
                      extent = 'geometry::EnvelopeAggregate(mupolygongeo).STAsText()',
                      convexhull = 'geometry::ConvexHullAggregate(mupolygongeo).STAsText()',
                      union = 'geometry::UnionAggregate(mupolygongeo).STAsText()',
                      point = 'mupolygongeo.STPointOnSurface().STAsText()')

  if (geom.src %in% c('sapolygon', 'featline', 'featpoint', 'mupoint', 'muline'))
    geom.type <- gsub('mupolygon', geom.src, geom.type)
  
  if (verbose)
    message(sprintf("Using %s chunks...", length(unique(mukey.chunk))))

  times <- vector(mode = "numeric", length = max(mukey.chunk))

  # discussion / testing related to optimal number ofgroups
  # https://github.com/ncss-tech/soilDB/issues/126

  for (i in unique(mukey.chunk)) {

    # thanks Kevin Wolz for pointing out the bug in chunk indexing
    idx <- which(mukey.chunk == i)
    mukeys <- mukey.list[idx]

    # SDA_query may generate a warning + try-error result
    chunk.res <- .fetchSDA_spatial(mukeys, geom.type, geom.src,
                                   use_statsgo, add.fields,
                                   verbose, i, by.col)

    # this almost always is because the query was too big
    # retry -- do each mukey individually
    if (inherits(chunk.res$result, 'try-error')) {
      # bad chunk
      subchunk.res <- lapply(mukeys, function(xx) {
        sub.res <- .fetchSDA_spatial(mukeys, geom.type, geom.src,
                                     use_statsgo, add.fields,
                                     verbose, paste0(i, "_", xx), by.col)
        if (inherits(sub.res$result, 'try-error')) {
          # explicit handling for a hypothetical unqueryable single mukey
          message("Symbol ", xx, " dropped from result due to error! May exceed the JSON serialization limit or have other topologic problems.")
          return(NULL)
        }
        return(sub.res)
      })

      # re-create full chunk from unit subchunks
      sub.res.res <- lapply(subchunk.res, function(x) if (!is.null(x)) x$result)
      sub.res.res.nul <- which(unlist(lapply(sub.res.res, is.null)))
      chunk.res$result <- do.call('rbind',  sub.res.res[sub.res.res.nul])
      chunk.res$time <- sum(unlist(lapply(subchunk.res, function(x) if (!is.null(x)) x$time)), na.rm = TRUE)
    }

    times[i] <- chunk.res$time

    # handle empty result
    if (!is.null(chunk.res$result)) {
      if (is.null(s)) {
         s <- chunk.res$result
      } else {
         s <- rbind(s, chunk.res$result)
      }
    }
  }

  tstop <- Sys.time()
  ttotdif <- difftime(tstop, tstart) # variable units
  mintime <- as.numeric(ttotdif, units = "mins") # minutes
  chunk.mean <- round(mean(as.numeric(times, units = "secs"), na.rm = TRUE), 1) # seconds
  mukey.mean <- round(as.numeric(mintime * 60 / length(mukey.list), units = "secs"), 2)

  if (verbose)
    message("Done in ", round(ttotdif, ifelse(attr(ttotdif,"units") == "secs", 1, 2)), " ",
          attr(ttotdif, "units"), "; mean/chunk: ", chunk.mean, " secs; ",
          "mean/symbol: ", mukey.mean, " secs", ".")
  if (!is.null(s)) {
    # store in result
    attr(s, "total.time") <- mintime
    attr(s, "mukey.mean") <- mukey.mean
    attr(s, "chunk.mean") <- chunk.mean
  }

  # flag for Spatial result
  if (as_Spatial && requireNamespace("sf")) {
    s <- sf::as_Spatial(s)
  }

  return(s)
}

.fetchSDA_spatial <- function(mukey.list, geom.type, geom.src, use_statsgo, add.fields, verbose, .parentchunk = NA, by.col) {
  base.fields <- "P.mukey, legend.areasymbol, mapunit.nationalmusym"
  
  if (geom.src %in% c("mupolygon", "mupoint", "muline")) {
    q <- sprintf(
      "SELECT
        %s AS geom, %s
      FROM %s AS P
        INNER JOIN mapunit ON P.mukey = mapunit.mukey
        INNER JOIN legend ON mapunit.lkey = legend.lkey
      WHERE P.mukey IN %s %s %s",
      geom.type,
      ifelse(grepl("Aggregate", geom.type), 
             "(SELECT STRING_AGG(value,', ') FROM 
               (SELECT DISTINCT value FROM STRING_SPLIT(STRING_AGG(CONVERT(NVARCHAR(max), P.mukey), ','),',')) t) AS mukey,
              (SELECT STRING_AGG(value,', ') FROM 
               (SELECT DISTINCT value FROM STRING_SPLIT(STRING_AGG(CONVERT(NVARCHAR(max), legend.areasymbol), ','),',')) t) AS areasymbol, 
              mapunit.nationalmusym", 
             "P.mukey, legend.areasymbol, mapunit.nationalmusym"),
      ifelse(use_statsgo, "gsmmupolygon", geom.src),
      format_SQL_in_statement(mukey.list),
      ifelse(use_statsgo, "AND CLIPAREASYMBOL = 'US'",""),
      ifelse(grepl("Aggregate", geom.type), 
             ifelse(by.col == "mukey", "GROUP BY P.mukey, mapunit.nationalmusym", "GROUP BY mapunit.nationalmusym"), "")
    )
  } else if (geom.src == "sapolygon") {
    
    base.fields <- "legend.areasymbol"
    
    q <- sprintf(
      "SELECT
         %s AS geom, legend.lkey, legend.areasymbol
       FROM sapolygon AS P
          INNER JOIN legend ON P.lkey = legend.lkey
       WHERE legend.lkey IN %s %s",
      geom.type,
      format_SQL_in_statement(mukey.list),
      ifelse(grepl("Aggregate", geom.type), "GROUP BY legend.lkey, legend.areasymbol", "")
    )
  } else if (geom.src == "featpoint" || geom.src == "featline") {
    q <- sprintf(
      "SELECT
         %s AS geom, areasymbol, featsym, featkey
       FROM %s AS P
       WHERE featkey IN %s %s",
      geom.type,
      geom.src,
      format_SQL_in_statement(mukey.list),
      ifelse(grepl("Aggregate", geom.type), "GROUP BY areasymbol, featsym, featkey", "")
    )
  }
  
  # add any additional fields from mapunit/legend
  if (!is.null(add.fields)) {
    q <- gsub(q, pattern = "FROM ([a-z]+)polygon",
              replacement = paste0(", ", paste0(ifelse(rep(grepl("Aggregate", geom.type), length(add.fields)),
               sprintf("(SELECT STRING_AGG(value,', ') FROM (SELECT DISTINCT value FROM STRING_SPLIT(STRING_AGG(CONVERT(NVARCHAR(max), %s), ','),',')) t) AS %s",
                       add.fields, gsub(".*\\.([a-z]+)", "\\1", add.fields)),
              add.fields), collapse = ", "), " FROM \\1polygon"))
  }
  t1 <- Sys.time()

  sp.res.sub <- try(suppressMessages(SDA_query(q)))

  if (inherits(sp.res.sub, 'try-error')) {
    message("Bad chunk encountered. Querying each individually...")
    return(list(result = sp.res.sub, time = NA))
  }

  s <- NULL
  if (!is.null(sp.res.sub)) {

    sp.res.sub$geom <- sf::st_as_sfc(sp.res.sub$geom, crs = 4326)
    s <- sf::st_as_sf(sp.res.sub) # sf::as_Spatial(sfobj)

    t2 <- Sys.time()
    tdif <- difftime(t2, t1, "secs")

    if (verbose)
      message("Chunk #",.parentchunk," completed (n = ",
              length(mukey.list), "; ", round(as.numeric(tdif),1), " secs)")

    times <- as.numeric(tdif, units = "secs")

  } else {

    times <- NA

    if (verbose)
      message("No spatial data found for: ",
              paste0(mukey.list, collapse = ","))
  }
  return(list(result = s, time = times))
}
