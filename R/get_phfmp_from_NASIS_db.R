get_phfmp_from_NASIS_db <- function(SS = TRUE, stringsAsFactors = NULL, dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  # because of alias with fetchNASIS cannot allow setting attr
  # also, attr is a free-form field, so not terribly useful -- consider SQL LIKE?

  #if(!is.null(attr) & length(attr)) {
  #  q <- paste0("SELECT * FROM phfmp_View_1 WHERE fmpname IN ",
  #              format_SQL_in_statement(attr),";")
  #} else {
    q <- "SELECT * FROM phfmp_View_1;"
  #}

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

  # field measured properties, long format
  return(uncode(d, dsn = dsn))
}
