get_phfmp_from_NASIS_db <- function(SS = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  # because of alias with fetchNASIS cannot allow setting attr
  # also, attr is a free-form field, so not terribly useful -- consider SQL LIKE?

  #if(!is.null(attr) & length(attr)) {
  #  q <- paste0("SELECT * FROM phfmp_View_1 WHERE fmpname IN ",
  #              format_SQL_in_statement(attr),";")
  #} else {
    q <- "SELECT * FROM phfmp_View_1;"
  #}

    channel <- .openNASISchannel()
    if (channel == -1)
      return(data.frame())

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec queries
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

  RODBC::odbcClose(channel)

  # field measured properties, long format
  return(uncode(d, stringsAsFactors = stringsAsFactors))
}
