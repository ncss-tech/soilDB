

#' @title Get RMF data from local NASIS
#' 
#' @description Prepare a list of `data.frame` objects with data from the "phrdxfeatures" and "phredoxfcolor" tables. These tables are related by "phrdxfiid" column, and related to horizon data via "phiid".
#'
#' @param SS logical, limit query to the selected set
#'
#' @param dsn optional path or _DBIConnection_ to \link[=NASISLocalDatabase]{local database containing NASIS table structure}; default: `NULL`
#' 
#' @return a `list` with two `data.frame` objects:
#'   * `RMF`: contents of "phrdxfeatures" table, often >1 row per horizon
#'   * `RMF_colors`: contents of "phredoxfcolor", usually >1 row per record in "phrdxfeatures"
#'   
#' @export 
get_RMF_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  # RMF
  # unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
  q <- "SELECT peiid, phiid,
  rdxfeatpct, rdxfeatsize, rdxfeatcntrst, rdxfeathardness, rdxfeatshape, rdxfeatkind, rdxfeatlocation, rdxfeatboundary, phrdxfiid
  FROM
  pedon_View_1
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phrdxfeatures_View_1 ON phorizon_View_1.phiid = phrdxfeatures_View_1.phiidref
  ORDER BY phiid, rdxfeatkind;"

  # RMF colors
  q.c <- "SELECT phrdxfiidref AS phrdxfiid,
  colorpct, colorhue, colorvalue, colorchroma, colormoistst
  FROM phredoxfcolor_View_1
  ORDER BY phrdxfiidref, colormoistst;"

  channel <- dbConnectNASIS(dsn)

  # error condition, empty DF
  # consider NULL
  if (inherits(channel, 'try-error')) {
    return(list(RMF = data.frame(), RMF_colors = data.frame()))
  }
    

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    q.c <- gsub(pattern = '_View_1', replacement = '', x = q.c, fixed = TRUE)
  }

  # exec queries
  d <- dbQueryNASIS(channel, q, close = FALSE)
  d.c <- dbQueryNASIS(channel, q.c)

  # convert coded -> text/factor representation of values
  d <- uncode(d, dsn = dsn)
  d.c <- uncode(d.c, dsn = dsn)

  # convert back to characters / numeric
  d.c$colormoistst <- as.character(d.c$colormoistst)
  d.c$colorhue <- as.character(d.c$colorhue)

  # uncode creates factors, so we have to convert to character first
  d.c$colorvalue <- as.numeric(as.character(d.c$colorvalue))
  d.c$colorchroma <- as.numeric(as.character(d.c$colorchroma))


  # done
  return(list(RMF = d, RMF_colors = d.c))
}
