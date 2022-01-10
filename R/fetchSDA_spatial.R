#' Get Spatial Data from Soil Data Access by `mukey`, `nationalmusym` or `areasymbol`
#'
#' @description This method facilitates queries to Soil Data Access (SDA) mapunit and survey area geometry. Queries are generated based on map unit key (`mukey`) and national map unit symbol (`nationalmusym`) for `mupolygon` (SSURGO) or `gsmmupolygon` (STATSGO) geometry OR legend key (`lkey`) and area symbols (`areasymbol`) for `sapolygon` (Soil Survey Area; SSA) geometry).
#'
#' A Soil Data Access query returns geometry and key identifying information about the map unit or area of interest. Additional columns from the map unit or legend table can be included; see `add.fields` argument.
#'
#' @param x A vector of map unit keys (`mukey`) or national map unit symbols (`nmusym`) for `mupolygon` geometry OR legend keys (`lkey`) or soil survey area symbols (`areasymbol`) for `sapolygon` geometry
#'
#' @param by.col Column name containing map unit identifier `"mukey"`, `"nmusym"`/`"nationalmusym"` for `geom.src` `mupolygon` OR `"areasymbol"`, `"areaname"`, `"mlraoffice"`, `"mouagncyresp"` for `geom.src` `sapolygon`; default is determined by `is.numeric(x)` `TRUE` for `mukey` or `lkey` and `nationalmusym` or `areasymbol` otherwise.
#'
#' @param method geometry result type: `"feature"` returns polygons, `"bbox"` returns the bounding box of each polygon (via `STEnvelope()`), and `"point"` returns a single point (via `STPointOnSurface()`) within each polygon.
#'
#' @param geom.src Either `mupolygon` (map unit polygons) or `sapolygon` (soil survey area boundary polygons)
#'
#' @param db Default: SSURGO. When `geom.src` is `mupolygon`, use STATSGO polygon geometry instead of SSURGO by setting `db = "STATSGO"`
#'
#' @param add.fields Column names from `mapunit` or `legend` table to add to result. Must specify parent table name as the prefix before column name e.g. `mapunit.muname`.
#'
#' @param chunk.size Number of values of `x` to process per query. Necessary for large results. Default: `10`
#'
#' @param verbose Print messages?
#'
#' @return A `Spatial*DataFrame` corresponding to SDA spatial data for all symbols requested. Default result contains geometry with attribute table containing unique feature ID, symbol and area symbol plus additional fields in result specified with `add.fields`.
#'
#' @details 
#' 
#' This function automatically "chunks" the input vector (using `makeChunks()`) of map unit identifiers to minimize the likelihood of exceeding the SDA data request size. The number of chunks varies with the `chunk.size` setting and the length of your input vector. If you are working with many map units and/or large extents, you may need to decrease this number in order to have more chunks.
#'
#' Querying regions with complex mapping may require smaller `chunk.size`. Numerically adjacent IDs in the input vector may share common qualities (say, all from same soil survey area or region) which could cause specific chunks to perform "poorly" (slow or error) no matter what the chunk size is. Shuffling the order of the inputs using `sample()` may help to eliminate problems related to this, depending on how you obtained your set of MUKEY/nationalmusym to query. One could feasibly use `muacres` as a heuristic to adjust for total acreage within chunks.
#' 
#' Note that STATSGO data are fetched where `CLIPAREASYMBOL = 'US'` to avoid duplicating state and national subsets of the geometry.
#' 
#' @author Andrew G. Brown, Dylan E. Beaudette
#'
#' @examples
#' \donttest{
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'
#'    # get spatial data for a single mukey
#'     single.mukey <- fetchSDA_spatial(x = "2924882")
#'
#'     # demonstrate fetching full extent (multi-mukey) of national musym
#'     full.extent.nmusym <- fetchSDA_spatial(x = "2x8l5", by = "nmusym")
#'
#'     # compare extent of nmusym to single mukey within it
#'     if(require(sp)) {
#'      plot(full.extent.nmusym, col = "RED",border=0)
#'      plot(single.mukey, add = TRUE, col = "BLUE", border=0)
#'     }
#'
#'     # demo adding a field (`muname`) to attribute table of result
#'     head(fetchSDA_spatial(x = "2x8l5", by="nmusym", add.fields="muname"))
#' }
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
                             verbose = TRUE) {
  db <- toupper(db)
  stopifnot(db %in% c("SSURGO", "STATSGO"))

  if (geom.src == 'sapolygon') {
    db <- 'SSURGO'
  }
  
  use_statsgo <- (db == "STATSGO")

  tstart <- Sys.time()

  # sanity check: method must be one of:
  if (!method %in% c('feature', 'bbox', 'point')) {
    stop('method must be one of: `feature`, `bbox`, or `point`.', call. = FALSE)
  }

  # remove any redundancy in input off the top -- this is important
  # in case x is not ordered and contains duplicates which will possibly
  # be in different chunks
  x <- unique(x)

  # lkey and areasymbol are the option for sapolygon
  if (geom.src == 'sapolygon' & (by.col %in% c("mukey", "nmusym", "nationalmusym"))) {
    if (is.numeric(x)) {
      by.col <- "lkey"
    } else {
      by.col <- "areasymbol"
    }
  }

  # default interface is mukey
  if (by.col == "mukey" | by.col == "lkey") {
    mukey.list <- x

  # a convenience interface is by nmusym -- may have several mukey per nmusym
  } else if (by.col == "nmusym" | by.col == "nationalmusym") {

    # do additional query to determine mapping of nmusym:mukey
    q.mukey <- paste0("SELECT nationalmusym, mukey FROM mapunit WHERE nationalmusym IN ",
                      format_SQL_in_statement(x),";")

    suppressMessages( {res <- SDA_query(q.mukey)} )

    if (inherits(res, 'try-error'))
      stop("fetchSDA_spatial: fatal error in national mapunit -> mukey conversion.", call. = FALSE)

    mukey.list <- unique(res$mukey)

  # a convenience interface for lkey is by areasymbol/areaname or other legend column
  } else if (by.col %in% c("areasymbol", "areasym", "areaname", "mlraoffice", "mouagncyresp")) {
    if (by.col == "areasym") by.col <- "areasymbol"
    
    if (by.col != "areasymbol") add.fields <- unique(c(add.fields, by.col))
    
    # do additional query to determine mapping of areasymbol:lkey
    q.mukey <- paste0("SELECT areasymbol, lkey FROM legend WHERE ", by.col, " IN ",
                      format_SQL_in_statement(x),";")

    suppressMessages( {res <- SDA_query(q.mukey)} )

    if (inherits(res, 'try-error'))
      stop("fetchSDA_spatial: fatal error in ", by.col, " -> lkey conversion.", call. = FALSE)

    mukey.list <- unique(res$lkey)
  } else {
    stop(paste0("Unknown mapunit identifier (",by.col,")"), call. = FALSE)
  }
  
  mukey.chunk <- makeChunks(mukey.list, chunk.size)
  
  s <- NULL

  # select method
  geom.type <- switch(method,
                      feature = 'mupolygongeo.STAsText()',
                      bbox = 'mupolygongeo.STEnvelope().STAsText()',
                      point = 'mupolygongeo.STPointOnSurface().STAsText()')

  if (geom.src == 'sapolygon')
    geom.type <- gsub('mupolygon', 'sapolygon', geom.type)

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
    chunk.res <- suppressWarnings(.fetchSDA_spatial(mukeys, geom.type, geom.src,
                                                    use_statsgo, add.fields,
                                                    verbose, i))

    # this almost always is because the query was too big
    # retry -- do each mukey individually
    if (inherits(chunk.res$result, 'try-error')) {
      # bad chunk
      subchunk.res <- lapply(mukeys, function(xx) {
        sub.res <- .fetchSDA_spatial(mukeys, geom.type, geom.src,
                                     use_statsgo, add.fields,
                                     verbose, paste0(i,"_",xx))

        if (inherits(sub.res$result, 'try-error')) {
          # explicit handling for a hypothetical unqueryable single mukey
          warning("Symbol ", xx, " dropped from result due to error! May exceed the JSON serialization limit or have other topologic problems.",
                  call. = FALSE)
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

  return(s)
}

.fetchSDA_spatial <- function(mukey.list, geom.type, geom.src, use_statsgo, add.fields, verbose, .parentchunk = NA) {
  if (geom.src == "mupolygon") {
    q <- sprintf(
      "SELECT
        %s AS geom,
        P.mukey, legend.areasymbol, mapunit.nationalmusym
      FROM %s AS P
        INNER JOIN mapunit ON P.mukey = mapunit.mukey
        INNER JOIN legend ON mapunit.lkey = legend.lkey
      WHERE P.mukey IN %s %s",
      geom.type,
      ifelse(use_statsgo, "gsmmupolygon", "mupolygon"),
      format_SQL_in_statement(mukey.list),
      ifelse(use_statsgo, "AND CLIPAREASYMBOL = 'US'","")
    )
  } else if (geom.src == "sapolygon") {
    q <- sprintf(
      "SELECT
         %s AS geom, legend.lkey, legend.areasymbol
       FROM sapolygon AS P
          INNER JOIN legend ON P.lkey = legend.lkey
       WHERE legend.lkey IN %s",
      geom.type,
      format_SQL_in_statement(mukey.list)
    )
  }
  # add any additional fields from mapunit/legend
  if (!is.null(add.fields)) {
    q <- gsub(q, pattern = "FROM ([a-z]+)polygon",
              replacement = paste0(", ", paste0(add.fields, collapse = ", "), " FROM \\1polygon"))
  }
  t1 <- Sys.time()

  sp.res.sub <- try(suppressMessages(soilDB::SDA_query(q)))

  if (inherits(sp.res.sub, 'try-error')) {
    message("Bad chunk encountered. Querying each individually...")
    return(list(result = sp.res.sub, time = NA))
  }

  s <- NULL
  if (!is.null(sp.res.sub)) {
    
    sp.res.sub$geom <- sf::st_as_sfc(wk::as_wkt(sp.res.sub$geom))
    sfobj <- sf::st_as_sf(sp.res.sub)
    sfobj <- sf::st_set_crs(sfobj, sf::st_crs(4326))
    s <- sf::as_Spatial(sfobj)
    
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
