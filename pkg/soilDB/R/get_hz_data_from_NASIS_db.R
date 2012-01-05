get_hz_data_from_NASIS_db <- function(dsn) {
  q <- "SELECT pedon.peiid, phorizon.phiid, pedon.upedonid as pedon_id,
  phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, CASE WHEN tx.ChoiceName IS NULL THEN til.ChoiceName  ELSE tx.ChoiceName END as texture_class, tmod.ChoiceName as texture_modifier, phfield, eff.ChoiceName AS effervescence, l.labsampnum, CASE WHEN f.total_frags_pct IS NULL THEN 0 ELSE f.total_frags_pct END AS total_frags_pct
  FROM (((((((
  (dbo.pedon INNER JOIN dbo.phorizon ON dbo.pedon.peiid = dbo.phorizon.peiidref) 
  LEFT OUTER JOIN (SELECT phiidref, SUM(fragvol) as total_frags_pct 
    FROM dbo.phfrags
    GROUP BY dbo.phfrags.phiidref) as f ON dbo.phorizon.phiid = f.phiidref) 
  LEFT OUTER JOIN (SELECT phiidref, labsampnum FROM dbo.phsample) as l ON dbo.phorizon.phiid = l.phiidref)
  LEFT OUTER JOIN (SELECT phtiid, phiidref, texcl, lieutex FROM dbo.phtexture) as t ON dbo.phorizon.phiid = t.phiidref)
  LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 189) AS tx ON t.texcl = tx.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 192) AS til ON t.lieutex = til.ChoiceValue)
  LEFT OUTER JOIN (SELECT phtiidref, texmod FROM dbo.phtexturemod) as tm ON t.phtiid = tm.phtiidref)
  LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 190) AS tmod ON tm.texmod = tmod.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 1255) AS eff ON dbo.phorizon.effclass = eff.ChoiceValue
  ORDER BY pedon.upedonid, phorizon.hzdept ASC;"
  
  # setup connection to our pedon database
  channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y')

  # exec query
  cat(paste('fetching from', 'local NASIS database', '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
  
  ## TODO test for non-uniqueness
  
  # close connection
  odbcClose(channel)
  
  # done
  return(d)
  }

