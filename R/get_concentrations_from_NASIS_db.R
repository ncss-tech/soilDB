get_concentrations_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = NULL, dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  # concentrations
  # unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
  q <- "SELECT peiid, phiid,
  concpct, concsize, conccntrst, conchardness, concshape, conckind, conclocation, concboundary, phconceniid
  FROM
  pedon_View_1
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phconcs_View_1 ON phorizon_View_1.phiid = phconcs_View_1.phiidref
  ORDER BY phiid, conckind;"

  # concentration colors
  q.c <- "SELECT phconceniidref AS phconceniid,
  colorpct, colorhue, colorvalue, colorchroma, colormoistst
  FROM phconccolor_View_1
  ORDER BY phconceniidref, colormoistst;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    q.c <- gsub(pattern = '_View_1', replacement = '', x = q.c, fixed = TRUE)
  }

  # exec queries
  d <- dbQueryNASIS(channel, q, close = FALSE)
  d.c <- dbQueryNASIS(channel, q.c)

  # uncode domained columns
  d <- uncode(d, dsn = dsn)
  d.c <- uncode(d.c, dsn = dsn)

  # convert back to characters / numeric
  d.c$colormoistst <- as.character(d.c$colormoistst)
  d.c$colorhue <- as.character(d.c$colorhue)

  # careful!
  # uncode creates factors, so we have to convert to character first
  d.c$colorvalue <- as.numeric(as.character(d.c$colorvalue))
  d.c$colorchroma <- as.numeric(as.character(d.c$colorchroma))

  # done
  return(list(conc = d, conc_colors = d.c))
}
