## Notes:
# source of the plantassocnm column changed - now resides in the siteobs table
# siteiidref key removed from pedon table - use the pedon.siteobsiidref through the siteobs table (siteobs.siteobsiid) as the new linkage	

## TODO: Does it matter whether the taxonkind is pulled from the pedon or petaxhistory tables? Should probably come from petaxhistory table

get_site_data_from_pedon_db <- function(dsn) {
  q <- "SELECT site.siteiid, pedon.peiid, upedonid as pedon_id, site.usiteid as site_id, siteobs.obsdate as obs_date,
  latdegrees + IIF(IsNull(latminutes), 0.0, latminutes/ 60.0) + IIF(IsNULL(latseconds), 0.0, latseconds / 60.0 / 60.0) as y,
  -(longdegrees + IIF(IsNull(longminutes), 0.0, longminutes / 60.0) + IIF(IsNull(longseconds), 0.0, longseconds / 60.0 / 60.0)) as x,
 dm.choice as datum, descname as describer, pp.choice as pedon_purpose, pt.choice as pedon_type, pedlabsampnum, psctopdepth, pscbotdepth, elev, slope, aspect, plantassocnm, bedrckdepth, br.choice_label as bedrock_kind, bh.choice_label as bedrock_hardness, hs.choice AS hillslope_pos 
FROM ((((((((
	site INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref) 
	LEFT OUTER JOIN pedon ON siteobs.siteobsiid = pedon.siteobsiidref) 
	LEFT OUTER JOIN sitebedrock ON site.siteiid = sitebedrock.siteiidref) 
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1261) AS dm ON site.horizdatnm = dm.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 517) AS br ON sitebedrock.bedrckkind = br.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1247) AS bh ON sitebedrock.bedrckhardness = bh.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1271) AS pp ON pedon.pedonpurpose = pp.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1273) AS pt ON pedon.pedontype = pt.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 971) AS hs ON site.hillslopeprof = hs.choice_id
  ORDER BY site.usiteid;"

  # setup connection to our pedon database
  channel <- odbcConnectAccess2007(dsn, readOnlyOptimize=TRUE)

  # exec query
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # warn if mixed datums
  unique.datums <- unique(na.omit(d$datum))
  if(length(unique.datums) > 1)
    message(paste('multiple datums present:', paste(unique.datums, collapse=', ')))
  
  # are there any dupes?
  t.pedon_id <- table(d$pedon_id)
  if(any(t.pedon_id > 1)) {
  	message('NOTICE: duplicate pedons:')
  	print(t.pedon_id[which(t.pedon_id > 1)])
  }
  
  # warn about sites without a matching pedon (records missing peiid)
  missing.pedon <- which(is.na(d$peiid))
  if(length(missing.pedon)> 0) {
  	message(paste('NOTICE: sites without pedons:', paste(unique(d$site_id[missing.pedon]), collapse=', ')))
  }
  
  # done
  return(d)
  }

