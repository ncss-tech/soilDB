#' Get mapunit ecological sites from Soil Data Access
#'
#' `get_SDA_coecoclass()` retrieves ecological site information from the Soil
#' Data Access (SDA) database for a given set of map unit keys (mukeys). It
#' returns a data frame containing ecological site IDs, names, and associated
#' classification details, enabling users to link soil map units to ecological
#' site concepts used in land management and conservation planning.
#'
#' @details When `method="Dominant Condition"` an additional field
#'   `ecoclasspct_r` is returned in the result with the sum of `comppct_r` that
#'   have the dominant condition `ecoclassid`. The component with the greatest
#'   `comppct_r` is returned for the `component` and `coecosite` level
#'   information.
#'
#'   Note that if there are multiple `coecoclasskey` per `ecoclassid` there may
#'   be more than one record per component.
#' @param method aggregation method. One of: `"Dominant Component"`, `"Dominant
#'   Condition"`, `"All"` or `"None"` (default). If `method="all"` multiple
#'   numbered columns represent site composition within each map unit e.g.
#'   `site1...`, `site2...`. If `method="none"` is selected one row will be
#'   returned per _component_; in all other cases one row will be returned per
#'   _map unit_.
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param WHERE character containing SQL WHERE clause specified in terms of
#'   fields in `legend`, `mapunit`, `component` or `coecosite` tables, used in
#'   lieu of `mukeys` or `areasymbols`
#' @param query_string Default: `FALSE`; if `TRUE` return a character string
#'   containing query that would be sent to SDA via `SDA_query`
#' @param ecoclasstypename Default: `c("NRCS Rangeland Site", "NRCS Forestland
#'   Site")`. If `NULL` no constraint on `ecoclasstypename` is used in the
#'   query.
#' @param ecoclassref Default: `"Ecological Site Description Database"`. If
#'   `NULL` no constraint on `ecoclassref` is used in the query.
#' @param not_rated_value Default: `"Not assigned"`
#' @param miscellaneous_areas logical. Include miscellaneous areas (non-soil
#'   components)?
#' @param include_minors logical. Include minor components? Default: `TRUE`.
#' @param threshold integer. Default: `0`. Minimum combined component percentage
#'   (RV) for inclusion of a mapunit's ecological site in wide-format tabular
#'   summary. Used only for `method="all"`.
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL`
#'   (default) use Soil Data Access API via `SDA_query()`.
#'
#' @export
#' @examplesIf !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)) 
#' # Basic usage with a vector of mukeys
#' get_SDA_coecoclass(mukeys = c(463994, 463995))
#'
#' # Using a custom WHERE clause (all "range" sites in Hawaii)
#' get_SDA_coecoclass(WHERE = "ecoclassid LIKE 'R%' AND areasymbol LIKE 'HI%'")
get_SDA_coecoclass <- function(method = "None",
                               areasymbols = NULL, mukeys = NULL, WHERE = NULL,
                               query_string = FALSE, 
                               ecoclasstypename = c("NRCS Rangeland Site", "NRCS Forestland Site"),
                               ecoclassref = "Ecological Site Description Database",
                               not_rated_value = "Not assigned",
                               miscellaneous_areas = TRUE,
                               include_minors = TRUE,
                               threshold = 0,
                               dsn = NULL) {
  
  method <- match.arg(toupper(method), c("NONE", "ALL", "DOMINANT COMPONENT", "DOMINANT CONDITION"))
  
  if (method == "ALL") {
    if (!is.null(WHERE)) {
      stop("`method='all'` does not support custom `WHERE` clauses", call. = FALSE)
    }
    
    if (isTRUE(query_string)) {
      stop("`method='all'` does not support `query_string=TRUE`", call. = FALSE)
    } 
    
    return(.get_SDA_coecoclass_agg(areasymbols = areasymbols, 
                                   mukeys = mukeys, 
                                   ecoclasstypename = ecoclasstypename,
                                   ecoclassref = ecoclassref,
                                   not_rated_value = not_rated_value,
                                   miscellaneous_areas = miscellaneous_areas,
                                   include_minors = include_minors,
                                   threshold = threshold,
                                   dsn = dsn))
  }
  
  if (!is.null(ecoclassref)) {
    ecoclassref_in <- soilDB::format_SQL_in_statement(ecoclassref)
  }
  
  if (!is.null(ecoclasstypename)) {
    ecoclasstypename_in <- soilDB::format_SQL_in_statement(ecoclasstypename)
  }
  
  base_query <- "SELECT mapunit.mukey, legend.areasymbol, legend.lkey, mapunit.muname, component.cokey, coecoclasskey, 
                  comppct_r, majcompflag, compname, localphase, compkind, ecoclassid, ecoclassname, ecoclasstypename, ecoclassref FROM legend
   LEFT JOIN mapunit ON legend.lkey = mapunit.lkey
   LEFT JOIN component ON mapunit.mukey = component.mukey %s
   LEFT JOIN coecoclass ON component.cokey = coecoclass.cokey %s
   WHERE %s"
  
  include_misc <- ifelse(miscellaneous_areas, "", " AND compkind != 'miscellaneous area'")
  include_mnrs <- ifelse(include_minors, "", " AND majcompflag = 'Yes'")
  
  include_src <- ifelse(is.null(ecoclassref), "", sprintf("(coecoclass.ecoclassref IS NULL OR coecoclass.ecoclassref IN %s)", ecoclassref_in))
  include_src2 <- ifelse(is.null(ecoclasstypename), "", sprintf("(coecoclass.ecoclasstypename IS NULL OR coecoclass.ecoclasstypename IN %s)", ecoclasstypename_in))
  
  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }

  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  } 
  
  foo <- ""
  if (include_src != "") {
    foo <- paste(foo, "AND", include_src)
  }
  
  if (include_src2 != "") {
    foo <- paste(foo, "AND", include_src2)
  }
  
  q <- sprintf(base_query, paste0(include_misc, include_mnrs), foo, WHERE)
  
  if (query_string)
    return(q)
  
  res <- SDA_query(q)
  
  if (length(res) == 0) {
    message('query returned no results')
    return(NULL)
  }
  
  .I <- NULL
  idx <- NULL
  comppct_r <- NULL
  ecoclasspct_r <- NULL
  
  res$ecoclassid[is.na(res$ecoclassid)] <- not_rated_value
  res$ecoclassname[is.na(res$ecoclassname)] <- not_rated_value
  if (length(ecoclasstypename) == 1) {
    # if only one type name requested, we can fill it in
    res$ecoclasstypename[is.na(res$ecoclasstypename)] <- ecoclasstypename
  } else {
    res$ecoclasstypename[is.na(res$ecoclasstypename)] <- not_rated_value
  }
  
  res$ecoclassref[is.na(res$ecoclassref)] <- not_rated_value
  
  if (method == "NONE") {
    return(res)
  } else if (method == "DOMINANT COMPONENT") {
    # dominant component
    idx1 <- data.table::data.table(res)[, .I[which.max(comppct_r)], by = "mukey"]$V1
    return(res[idx1, ])
  } 
  
  idx2 <- data.table::data.table(res)[, list(idx = .I[which.max(comppct_r)],
                                             ecoclasspct_r = sum(comppct_r)), 
                                      by = c("mukey", "ecoclassid")][,
                                        list(idx = idx[which.max(ecoclasspct_r)],
                                             ecoclasspct_r = ecoclasspct_r[which.max(ecoclasspct_r)]), 
                                      by = "mukey"]
  res2 <- res[idx2$idx, ]
  res2$ecoclasspct_r <- idx2$ecoclasspct_r
  res2
}

.get_SDA_coecoclass_agg <- function(areasymbols = NULL,
                                    mukeys = NULL,
                                    ecoclasstypename = c("NRCS Rangeland Site", "NRCS Forestland Site"),
                                    ecoclassref = "Ecological Site Description Database",
                                    not_rated_value = "Not assigned",
                                    miscellaneous_areas = TRUE,
                                    include_minors = TRUE,
                                    dsn = NULL,
                                    threshold = 0) {
                                      
  comppct_r <- NULL; condpct_r <- NULL; compname <- NULL; # ecoclasstypename <- NULL
  areasymbol <- NULL; compnames <- NULL; unassigned <- NULL;
  mukey <- NULL; .N <- NULL; .SD <- NULL; .GRP <- NULL;
  
  if (!is.null(areasymbols)) {
    res0 <- SDA_query(paste0(
        "SELECT DISTINCT mukey, nationalmusym, muname FROM mapunit
        INNER JOIN legend ON legend.lkey = mapunit.lkey
        WHERE areasymbol IN ", format_SQL_in_statement(areasymbols)
      ))
    idx <- makeChunks(res0$mukey, 1000)
    l <- split(res0$mukey, idx)
  } else {
    idx <- makeChunks(mukeys, 1000)
    l <- split(mukeys, idx)
    res0 <- do.call('rbind', lapply(l, function(x) {
      SDA_query(paste0(
        "SELECT DISTINCT mukey, nationalmusym, muname FROM mapunit
        INNER JOIN legend ON legend.lkey = mapunit.lkey
        WHERE mukey IN ", format_SQL_in_statement(x), ""
      ))
    }))
    idx <- makeChunks(res0$mukey, 1000)
    l <- split(res0$mukey, idx)
  }
  
  res1 <- do.call('rbind', lapply(l, function(x) {
    get_SDA_coecoclass(
      mukeys = x,
      method = "None",
      ecoclasstypename = ecoclasstypename,
      ecoclassref = ecoclassref,
      not_rated_value = not_rated_value,
      miscellaneous_areas = miscellaneous_areas,
      include_minors = include_minors,
      dsn = dsn
    )
  }))
  
  res1 <- data.table::data.table(res1)[, .SD[order(comppct_r, decreasing = TRUE), ], by = "mukey"]
  res2 <- data.table::data.table(subset(res1, areasymbol != "US"))
  
  # remove FSG etc. some components have no ES assigned, but have other eco class
  idx <- !res2$ecoclassref %in% c(not_rated_value, "Not assigned", ecoclassref) &
    !res2$ecoclasstypename %in% c(not_rated_value, "Not assigned", ecoclasstypename)
  
  res2$ecoclassid[idx] <- not_rated_value
  res2$ecoclassref[idx] <- not_rated_value
  res2$ecoclassname[idx] <- not_rated_value
  res2$ecoclasstypename[idx] <- not_rated_value
  
  .ECOCLASSTYPENAME <- ecoclasstypename
  
  res3 <- res2[, list(
    condpct_r = sum(comppct_r, na.rm = TRUE),
    compnames = toString(compname[ecoclasstypename %in% .ECOCLASSTYPENAME]),
    unassigned = toString(compname[!ecoclasstypename %in% .ECOCLASSTYPENAME])
  ), by = c("mukey", "ecoclassid", "ecoclassname")][, rbind(
    .SD[, 1:4],
    data.frame(
      ecoclassid = not_rated_value,
      ecoclassname = not_rated_value,
      condpct_r = sum(condpct_r[nchar(unassigned) > 0 & !unassigned %in% compnames], na.rm = TRUE),
      compnames = toString(unassigned[nchar(unassigned) > 0 & !unassigned %in% compnames])
    )
  ), by = c("mukey")][order(mukey, -condpct_r), ]
  res3 <- res3[nchar(res3$compnames) > 0,]
  
  # could do up to max_sites, but generally cut to some minimum condition percentage `threshold`
  max_sites <- suppressWarnings(max(res3[, .N, by = "mukey"]$N))
  res3 <- res3[res3$condpct_r >= threshold, ]
  max_sites_pruned <- suppressWarnings(max(res3[, .N, by = "mukey"]$N))
  res3$condpct_r <- as.integer(res3$condpct_r)
  
  if (max_sites > max_sites_pruned) {
    message("maximum number of sites per mukey: ", max_sites)
    message("using maximum number of sites above threshold (",
            threshold, "%) per mukey: ", max_sites_pruned)
  }
  
  if (!is.finite(max_sites_pruned)) {
    sdx <- 1
  } else {
    sdx <- seq(max_sites_pruned)
  }
  
  .coecoclass_long_to_wide <- function(x, group) {
    res <- data.frame(grpid = group)
    for (i in sdx) {
      if (i > nrow(x)) {
        d <- data.frame(
          siten = not_rated_value,
          sitenname = not_rated_value,
          sitencompname = NA_character_,
          sitenpct_r = NA_integer_,
          sitenlink = NA_character_
        )
      } else {
        d <- data.frame(
          siten = ifelse(isTRUE(is.na(x$ecoclassid[i])), not_rated_value, x$ecoclassid[i]),
          sitenname = ifelse(isTRUE(is.na(x$ecoclassname[i])), not_rated_value, x$ecoclassname[i]),
          sitencompname = ifelse(isTRUE(is.na(x$compnames[i])), NA_character_, x$compnames[i]),
          sitenpct_r = ifelse(isTRUE(is.na(x$condpct_r[i])), 0L, x$condpct_r[i]),
          sitenlink = ifelse(
            isTRUE(x$ecoclassid[i] == not_rated_value | is.na(x$ecoclassid[i])),
            NA_character_,
            paste0(
              "https://edit.jornada.nmsu.edu/catalogs/esd/",
              substr(x$ecoclassid[i], 2, 5),
              "/",
              x$ecoclassid[i]
            )
          )
        )
        # sitenpdf = ifelse(isTRUE(x$ecoclassid[i] == "Not assigned"), NA_character_,
        #                   paste0(
        #                     "https://edit.jornada.nmsu.edu/services/descriptions/esd/",
        #                     substr(x$ecoclassid[i], 2, 5), "/", x$ecoclassid[i], ".pdf"
        #                   )))
      }
      colnames(d) <- c(
        paste0("site", i), paste0("site", i, "name"), paste0("site", i, "compname"),
        paste0("site", i, "pct_r"), paste0("site", i, "link")
      )
      res <- cbind(res, d)
    }
    res
  }
  
  res <- merge(data.table::data.table(res0), res3, by = "mukey", all.x = TRUE)[, 
          .coecoclass_long_to_wide(.SD, .GRP), 
          by = c("mukey", "muname", "nationalmusym"), ]
  
  res$grpid <- NULL
  res
}

