get_hz_data_from_pedon_db <-
function(dsn)
  {
  require(RODBC)
  
  # this can be optimized
  # RF calculation should be done in  a sub-query
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id,
  phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, phfield, 
  IIF(IsNULL(f.total_frags_pct), 0, f.total_frags_pct) AS total_frags_pct
  FROM (
  (pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref) 
  LEFT OUTER JOIN (
    SELECT phiidref, SUM(fragvol) as total_frags_pct 
    FROM phfrags
    GROUP BY phiidref
    ) as f on phorizon.phiid = f.phiidref
  )
  ORDER BY pedon.upedonid, phorizon.hzdept ASC ;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # done
  return(d)
  }

