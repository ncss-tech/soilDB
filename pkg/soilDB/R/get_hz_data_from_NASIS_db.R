get_hz_data_from_NASIS_db <- function(dsn)
  {
  # this can be optimized
  # RF calculation should be done in  a sub-query
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id,
  phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, phfield, 
  CASE WHEN f.total_frags_pct IS NULL THEN 0 ELSE f.total_frags_pct END AS total_frags_pct
  FROM (
  (dbo.pedon INNER JOIN dbo.phorizon ON dbo.pedon.peiid = dbo.phorizon.peiidref) 
  LEFT OUTER JOIN (
    SELECT phiidref, SUM(fragvol) as total_frags_pct 
    FROM dbo.phfrags
    GROUP BY dbo.phfrags.phiidref
    ) as f on dbo.phorizon.phiid = f.phiidref
  )
  ORDER BY pedon.upedonid, phorizon.hzdept ASC ;"
  
  # setup connection to our pedon database
  channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y')

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # done
  return(d)
  }

