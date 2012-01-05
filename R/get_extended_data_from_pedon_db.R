get_extended_data_from_pedon_db <- function(dsn) {
  # query diagnostic horizons, usually a 1:many relationship with pedons
  q.diagnostic <- "SELECT peiidref as peiid, dfk.choice as diag_kind, featdept, featdepb
FROM pediagfeatures
LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 147) AS dfk ON pediagfeatures.featkind = dfk.choice_id
ORDER BY pediagfeatures.peiidref, pediagfeatures.featdept;"
  
  
  # this query is resistant to dupes
  # query rock-fragment summary by horizon
  q.rf.summary <- "SELECT phfrags.phiidref as phiid, IIF(IsNULL(f1.gravel), 0.0, f1.gravel) as gravel, IIF(IsNULL(f2.cobbles), 0.0, f2.cobbles) as cobbles, IIF(IsNULL(f3.stones), 0.0, f3.stones) as stones, IIF(IsNULL(f4.boulders), 0.0, f4.boulders) as boulders, IIF(IsNULL(f5.channers), 0.0, f5.channers) as channers, IIF(IsNULL(f6.flagstones), 0.0, f6.flagstones) as flagstones
FROM (((((((phorizon INNER JOIN phfrags ON phorizon.phiid = phfrags.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS gravel
  FROM phfrags
  WHERE (((phfrags.fragsize_h)<=76) OR ((phfrags.fragsize_r)<=76 And (phfrags.fragsize_r)>=2))
  GROUP BY phfrags.phiidref) as f1 ON phfrags.phiidref = f1.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS cobbles
  FROM phfrags
  WHERE (((phfrags.fragsize_l)>=75) AND ((phfrags.fragsize_h)<=250)) OR (((phfrags.fragsize_r)>=76 And (phfrags.fragsize_r)<=250))
  GROUP BY phfrags.phiidref) as f2 ON phfrags.phiidref = f2.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS stones
  FROM phfrags
  WHERE (((phfrags.fragsize_l)>=250) OR ((phfrags.fragsize_r)>=250) OR (((phfrags.fragsize_l)>=380) AND (phfrags.fragshp)=1) OR   (((phfrags.fragsize_r)>=380) AND ((phfrags.fragshp)=1)))
  GROUP BY phfrags.phiidref) as f3 ON phfrags.phiidref = f3.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS boulders
  FROM phfrags
  WHERE (((phfrags.fragsize_l)>=600) OR (((phfrags.fragsize_r)>=600)  AND ((phfrags.fragshp)=1)))
  GROUP BY phfrags.phiidref) as f4 ON phfrags.phiidref = f4.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS channers
  FROM phfrags
  WHERE (((phfrags.fragsize_l)>=2) AND ((phfrags.fragsize_h)<=150) AND ((phfrags.fragshp)=1)) OR (((phfrags.fragshp)=1) AND ((phfrags.fragsize_r)>=2 And (phfrags.fragsize_r)<=150))
  GROUP BY phfrags.phiidref) as f5 ON phfrags.phiidref = f5.phiidref)
LEFT OUTER JOIN (SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS flagstones
  FROM phfrags
  WHERE (((phfrags.fragsize_l)>=150) AND ((phfrags.fragsize_h)<=380) AND ((phfrags.fragshp)=1)) OR (((phfrags.fragshp)=1) AND   ((phfrags.fragsize_r)>=150 And (phfrags.fragsize_r)<=380))
  GROUP BY phfrags.phiidref) as f6 ON phfrags.phiidref = f6.phiidref)
GROUP BY phfrags.phiidref, gravel, cobbles, stones, boulders, channers, flagstones
ORDER BY phfrags.phiidref;"
  
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d.diagnostic <- sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
  d.rf.summary <- sqlQuery(channel, q.rf.summary, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  
  # return a list of results
  return(list(diagnostic=d.diagnostic, frag_summary=d.rf.summary))
  }

