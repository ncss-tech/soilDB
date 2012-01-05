get_extended_data_from_NASIS_db <- function(dsn) {
  # query diagnostic horizons, usually a 1:many relationship with pedons
  q.diagnostic <- "SELECT peiidref as peiid, dfk.ChoiceName as diag_kind, featdept, featdepb
FROM dbo.pediagfeatures 
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE DomainID = 147) AS dfk ON dbo.pediagfeatures.featkind = dfk.ChoiceValue
ORDER BY dbo.pediagfeatures.peiidref, dbo.pediagfeatures.featdept;"
  
  
  # this query is resistant to dupes
  # query rock-fragment summary by horizon
  q.rf.summary <- "SELECT dbo.phfrags.phiidref as phiid, CASE WHEN f1.gravel IS NULL THEN 0.0 ELSE f1.gravel END as gravel, CASE WHEN f2.cobbles IS NULL THEN 0.0 ELSE f2.cobbles END as cobbles, CASE WHEN f3.stones IS NULL THEN 0.0 ELSE f3.stones END as stones, CASE WHEN f4.boulders IS NULL THEN 0.0 ELSE f4.boulders END as boulders, CASE WHEN f5.channers IS NULL THEN 0.0 ELSE f5.channers END as channers, CASE WHEN f6.flagstones IS NULL THEN 0.0 ELSE f6.flagstones END as flagstones
FROM (((((((dbo.phorizon INNER JOIN dbo.phfrags ON dbo.phorizon.phiid = dbo.phfrags.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS gravel
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_h)<=76) OR ((dbo.phfrags.fragsize_r)<=76 And (dbo.phfrags.fragsize_r)>=2))
  GROUP BY dbo.phfrags.phiidref) as f1 ON dbo.phfrags.phiidref = f1.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS cobbles
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_l)>=75) AND ((dbo.phfrags.fragsize_h)<=250)) OR (((dbo.phfrags.fragsize_r)>=76 And (dbo.phfrags.fragsize_r)<=250))
  GROUP BY dbo.phfrags.phiidref) as f2 ON dbo.phfrags.phiidref = f2.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS stones
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_l)>=250) OR ((dbo.phfrags.fragsize_r)>=250) OR (((dbo.phfrags.fragsize_l)>=380) AND (dbo.phfrags.fragshp)=1) OR   (((dbo.phfrags.fragsize_r)>=380) AND ((dbo.phfrags.fragshp)=1)))
  GROUP BY dbo.phfrags.phiidref) as f3 ON dbo.phfrags.phiidref = f3.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS boulders
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_l)>=600) OR (((dbo.phfrags.fragsize_r)>=600)  AND ((dbo.phfrags.fragshp)=1)))
  GROUP BY dbo.phfrags.phiidref) as f4 ON dbo.phfrags.phiidref = f4.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS channers
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_l)>=2) AND ((dbo.phfrags.fragsize_h)<=150) AND ((dbo.phfrags.fragshp)=1)) OR (((dbo.phfrags.fragshp)=1) AND   ((dbo.phfrags.fragsize_r)>=2 And (dbo.phfrags.fragsize_r)<=150))
  GROUP BY dbo.phfrags.phiidref) as f5 ON dbo.phfrags.phiidref = f5.phiidref)
LEFT OUTER JOIN (SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS flagstones
  FROM dbo.phfrags
  WHERE (((dbo.phfrags.fragsize_l)>=150) AND ((dbo.phfrags.fragsize_h)<=380) AND ((dbo.phfrags.fragshp)=1)) OR (((dbo.phfrags.fragshp)=1) AND   ((dbo.phfrags.fragsize_r)>=150 And (dbo.phfrags.fragsize_r)<=380))
  GROUP BY dbo.phfrags.phiidref) as f6 ON dbo.phfrags.phiidref = f6.phiidref)
GROUP BY dbo.phfrags.phiidref, gravel, cobbles, stones, boulders, channers, flagstones
ORDER BY dbo.phfrags.phiidref;"
  
  
  # setup connection to our local NASIS database
  channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y') 

  # exec queries
  cat(paste('fetching from', dsn, '...\n'))
  d.diagnostic <- sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
  d.rf.summary <- sqlQuery(channel, q.rf.summary, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  
  # return a list of results
  return(list(diagnostic=d.diagnostic, frag_summary=d.rf.summary))
  }

