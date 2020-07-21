#' @title Query SDA and Return Spatial Data
#' 
#' @description This is a high-level "fetch" method to facilitate spatial queries to Soil Data Access (SDA) based on \code{mukey} or \code{nationalmusym}. 
#' 
#' A SDA spatial query is made returning geometry and key identifying information about the mapunit. Additional columns from the mapunit table can be included using \code{add.fields} argument. 
#' 
#' This function automatically "chunks" the input vector (using \code{soilDB::makeChunks}) of mapunit identifiers to minimize the likelihood of exceeding the SDA data request size. The number of chunks varies with the \code{chunk.size} setting and the length of your input vector. If you are working with many mapunits and/or large extents, you may need to decrease this number in order to have more chunks.
#' 
#' The "sweet-spot" \code{chunk.size} should optimize number of queries relative to the typical amount of information in each query. There is a 100,000 record limit per query and the type / complexity of the geometry affects the total result size; there is a ~32Mb JSON serialization limit.
#' 
#' Querying regions with complex mapping may require smaller \code{chunk.size}. Numerically adjacent IDs in the input vector may share common qualities (say, all from same soil survey area or region) which could cause specific chunks to perform "poorly" [slow or error] no matter what the chunk size is. Shuffling the order of the inputs using \code{sample} may help to eliminate problems related to this, depending on how you obtained your set of MUKEY/nationalmusym to query. One could feasibly use \code{muacres} as a heuristic to adjust for total acreage within chunks.
#' 
#' @param x A vector of MUKEYs or national mapunit symbols.
#' @param by.col Column name containing mapunit identifier ("mukey" or "nmusym"); default: "mukey"
#' @param method geometry result type: 'feature' returns polygons, 'bbox' returns the bounding box of each polygon, and 'point' returns a single point within each polygon.
#' @param add.fields Column names from `mapunit` table to add to result. Must specify table name prefix `mapunit` before column name (e.g. `mapunit.muname`).
#' @param chunk.size How many queries should spatial request be divided into? Necessary for large results. Default: 10
#' @param verbose Print messages?
#' 
#' @return A Spatial*DataFrame corresponding to SDA spatial data for all MUKEYs / nmusyms requested. Default result contains mapunit delineation geometry with attribute table containing `gid`, `mukey` and `nationalmusym`, plus additional fields in result specified with `add.fields`.
#' 
#' @author Andrew G. Brown
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
fetchSDA_spatial <- function(x, by.col = "mukey", method = 'feature',
                             add.fields = NULL, chunk.size = 10, verbose = TRUE) {
  tstart <- Sys.time()
  
  # sanity check: method must be one of:
  if (!method %in% c('feature', 'bbox', 'point')) {
    stop('method must be one of: `feature`, `bbox`, or `point`.', call. = FALSE)
  }
  
  # remove any redundancy in input off the top -- this is important
  # in case x is not ordered and contains duplicates which will possibly
  # be in different chunks
  x <- unique(x)
  
  # default interface is mukey
  if (by.col == "mukey") {
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
    
  # currently only mukey and nmusym are supported
  } else {
    stop(paste0("Unknown mapunit identifier (",by.col,")"), call. = FALSE)
  }
  
  mukey.chunk <- soilDB::makeChunks(mukey.list, chunk.size)
  s <- NULL
  
  # select method
  geom.type <- switch(method, 
                      feature = 'mupolygongeo.STAsText()',
                      bbox = 'mupolygongeo.STEnvelope().STAsText()',
                      point = 'mupolygongeo.STPointOnSurface().STAsText()')
  
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
    chunk.res <- suppressWarnings(.fetchSDA_spatial(mukeys, geom.type, add.fields, verbose, i))
    
    # this almost always is because the query was too big
    # retry -- do each mukey individually
    if (inherits(chunk.res$result, 'try-error')) {
      # bad chunk 
      subchunk.res <- lapply(mukeys, function(xx) {
        sub.res <- .fetchSDA_spatial(xx, geom.type, add.fields, verbose, paste0(i,"_",xx))
        
        if (inherits(sub.res, 'try-error')) {
          # explicit handling for a hypothetical unqueryable single mukey
          warning("MUKEY ", xx, " dropped from result due to error! This MUKEY may exceed the JSON serialization limit or have other topologic problems.")
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
          "mean/mukey: ", mukey.mean, " secs", ".")
  
  # store in result
  attr(s, "total.time") <- mintime
  attr(s, "mukey.mean") <- mukey.mean
  attr(s, "chunk.mean") <- chunk.mean
  
  return(s)
}

.fetchSDA_spatial <- function(mukey.list, geom.type, add.fields, verbose, .parentchunk = NA) {
  q <- sprintf(
    "SELECT 
          %s AS geom, 
          P.mukey, mapunit.nationalmusym
          FROM mupolygon AS P
          INNER JOIN mapunit ON P.mukey = mapunit.mukey
          WHERE mapunit.mukey IN %s",
    geom.type,
    format_SQL_in_statement(mukey.list)
  )
  
  # add any additional fields from mapunit
  if (!is.null(add.fields)) {
    q <- gsub(q, pattern = "FROM mupolygon", 
              replacement = paste0(", ", paste0(add.fields, collapse = ", "), " FROM mupolygon"))
  }
  t1 <- Sys.time()
  
  sp.res.sub <- try(suppressMessages(soilDB::SDA_query(q)))
  
  if (inherits(sp.res.sub, 'try-error')) {
    message("Bad chunk encountered. Querying each MUKEY individually...")
    return(list(result = sp.res.sub, time = NA))
  }
  
  s <- NULL
  if (!is.null(sp.res.sub)) {
    
    s <- soilDB::processSDA_WKT(sp.res.sub)

    
    t2 <- Sys.time()
    tdif <- difftime(t2, t1, "secs")
    
    if (verbose)
      message("Chunk #",.parentchunk," completed (n_mukey = ",
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
