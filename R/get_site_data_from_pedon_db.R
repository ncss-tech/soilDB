get_site_data_from_pedon_db <- function(dsn) {
  q <- "SELECT site.siteiid, pedon.peiid, site.usiteid as site_id, siteobs.obsdate as obs_date,
  latdegrees + IIF(IsNull(latminutes), 0.0, latminutes/ 60.0) + IIF(IsNULL(latseconds), 0.0, latseconds / 60.0 / 60.0) as y,
  -(longdegrees + IIF(IsNull(longminutes), 0.0, longminutes / 60.0) + IIF(IsNull(longseconds), 0.0, longseconds / 60.0 / 60.0)) as x,
  dm.choice as datum, descname as describer, pp.choice as pedon_purpose, soinmassamp as sampled_as, soinmascorr as correlated_as, pedlabsampnum, psctopdepth, pscbotdepth, ps.choice_label as part_size_class, ts.choice_label as tax_subgroup, elev, slope, aspect, plantassocnm, bedrckdepth, br.choice_label as bedrock_kind, hs.choice AS hillslope_pos 
  FROM (
  (
  (
  (
  (
  (
  (site INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref) LEFT OUTER JOIN pedon ON site.siteiid = pedon.siteiidref)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1261) AS dm ON site.horizdatnm = dm.choice_id) 
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 517) AS br ON site.bedrckkind = br.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 127) AS ps ON pedon.taxpartsize = ps.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 187) AS ts ON pedon.taxsubgrp = ts.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1271) AS pp ON pedon.pedonpurpose = pp.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 971) AS hs ON site.hillslopeprof = hs.choice_id
  ORDER BY site.usiteid;"

 #q <- "SELECT siteobs.obsdate as obs_date 
 #FROM (site INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref) LEFT OUTER JOIN pedon ON site.siteiid = pedon.siteiidref;"
 #(site INNER JOIN (siteobs LEFT JOIN pedon ON site.siteiid = pedon.siteiidref) ON site.siteiid = siteobs.siteiidref)
 
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # warn if mixed datums
  if(length(unique(na.omit(d$datum))) > 1)
    warning('multiple datums present')
  
  # are there any dupes?
  if(any(table(d$pedon_id) > 1))
    warning('duplicate site/pedon information in results')
  
  # done
  return(d)
  }

