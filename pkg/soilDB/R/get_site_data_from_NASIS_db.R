get_site_data_from_NASIS_db <- function(dsn) {
	q <- "SELECT dbo.site.siteiid as siteiid, dbo.pedon.peiid, dbo.site.usiteid as site_id, dbo.pedon.upedonid as pedon_id, dbo.siteobs.obsdate as obs_date, -(longdegrees + CASE WHEN longminutes IS NULL THEN 0.0 ELSE longminutes / 60.0 END + CASE WHEN longseconds IS NULL THEN 0.0 ELSE longseconds / 60.0 / 60.0 END) as x, latdegrees + CASE WHEN latminutes IS NULL THEN 0.0 ELSE latminutes / 60.0 END + CASE WHEN latseconds IS NULL THEN 0.0 ELSE latseconds / 60.0 / 60.0 END as y, dm.ChoiceName as datum, dbo.pedon.descname as describer, pp.ChoiceName as pedon_purpose, pt.ChoiceName as pedon_type, soinmassamp as sampled_as, soinmascorr as correlated_as, pedlabsampnum, psctopdepth, pscbotdepth, ps.ChoiceLabel as part_size_class, ts.ChoiceLabel as tax_subgroup, elev, slope, aspect, plantassocnm, bedrckdepth, br.ChoiceLabel as bedrock_kind, hs.ChoiceLabel as hillslope_pos
FROM
(
(
(
(
(
( 
(dbo.site INNER JOIN (dbo.siteobs LEFT JOIN dbo.pedon ON dbo.siteobs.siteobsiid = dbo.pedon.siteobsiidref) ON dbo.site.siteiid = dbo.siteobs.siteiidref)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 1261) AS dm ON dbo.site.horizdatnm = dm.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 517) AS br ON dbo.site.bedrckkind = br.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 127) AS ps ON dbo.pedon.taxpartsize = ps.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 187) AS ts ON dbo.pedon.taxsubgrp = ts.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 1271) AS pp ON dbo.pedon.pedonpurpose = pp.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 1273) AS pt ON dbo.pedon.pedontype = pt.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 971) AS hs ON dbo.site.hillslopeprof = hs.ChoiceValue
ORDER BY dbo.site.usiteid ;"
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y') 
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# warn if mixed datums
	if(length(unique(na.omit(d$datum))) > 1)
		cat('NOTICE: multiple datums present\n')
	
	# are there any dupelicate pedon IDs?
	t.pedon_id <- table(d$pedon_id)
	if(any(t.pedon_id > 1)) {
		cat('NOTICE: duplicate pedons')
		print(t.pedon_id[which(t.pedon_id > 1)])
	}
	
	
	# done
	return(d)
}

