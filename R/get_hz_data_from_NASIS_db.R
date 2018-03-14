## TODO: when multiple textures have been defined, only the first one is returned (alphabetical ?)
# 
get_hz_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT peiid, phiid, upedonid as pedon_id,
  hzname, dspcomplayerid as genhz, hzdept, hzdepb,
  claytotest AS clay, CASE WHEN silttotest IS NULL THEN 100 - (claytotest + sandtotest) ELSE silttotest END AS silt, 
  sandtotest AS sand, fragvoltot, texture, texcl, lieutex, phfield, effclass, phs.labsampnum, rupresblkdry, stickiness, plasticity
  
  FROM 

  pedon_View_1 p 
  INNER JOIN phorizon_View_1 ph ON ph.peiidref = p.peiid 
  LEFT OUTER JOIN phsample_View_1 phs ON phs.phiidref = ph.phiid
  LEFT OUTER JOIN 
  (
  SELECT phiidref, MIN(texcl) AS texcl, MIN(lieutex) as lieutex
  FROM phtexture_View_1 
  GROUP BY phiidref
  ) AS pht ON pht.phiidref = ph.phiid
  
  ORDER BY p.upedonid, ph.hzdept ASC;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  
  # exec query
  d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # re-implement texture_class column, with lieutex in cases where texcl is missing
  d$texture_class <- ifelse(is.na(d$texcl) & ! is.na(d$lieutex), as.character(d$lieutex), as.character(d$texcl))
  
  # test for duplicate horizons:
  #  bugs in our queries
  #   multiple lab samples / genetic horizon
  hz.tab <- table(d$phiid)
  dupe.hz <- which(hz.tab > 1)
  dupe.hz.phiid <- names(hz.tab[dupe.hz])
  dupe.hz.pedon.ids <- d$pedon_id[d$phiid %in% dupe.hz.phiid]
  
  if (length(dupe.hz) > 0) {
    message(paste('NOTICE: multiple `labsampnum` values / horizons; see pedon IDs:\n', paste(unique(dupe.hz.pedon.ids), collapse=','), sep=''))
  }
  
  # close connection
  RODBC::odbcClose(channel)
  
  # done
  return(d)
}

