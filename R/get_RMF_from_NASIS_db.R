get_RMF_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

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

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
    q.c <- gsub(pattern = '_View_1', replacement = '', x = q.c, fixed = TRUE)
  }

  # exec queries
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  d.c <- RODBC::sqlQuery(channel, q.c, stringsAsFactors=FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # uncode domained columns
  d <- uncode(d)
  d.c <- uncode(d.c)

  # convert back to characters / numeric
  d.c$colormoistst <- as.character(d.c$colormoistst)
  d.c$colorhue <- as.character(d.c$colorhue)
  # careful!
  # uncode creates factors, so we have to convert to character first
  d.c$colorvalue <- as.numeric(as.character(d.c$colorvalue))
  d.c$colorchroma <- as.numeric(as.character(d.c$colorchroma))


  # done
  return(list(RMF=d, RMF_colors=d.c))
}
