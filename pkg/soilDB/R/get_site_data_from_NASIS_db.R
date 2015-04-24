## TODO: temporary hack to deal with the possibility of multiple site-bedrock entries:
# row number is computed for the site-bedrock table... we can't depend on the verticle order column to contain useful information
# we then filter on sb.rn at the end of the query. 
# this same syntax will not work in Access

## further ideas:
# http://stackoverflow.com/questions/3800551/select-first-row-in-each-group-by-group
# ... this would be a lot cleaner if we used WITH ... to define all sub-tables

## TODO: spatial data will likely be referenced to multiple datums... 
# the STD coordiants in NASIS are WGS84, but have to be manually "calculated"
# see: Import of Standard WGS84 Georeference
# for now, 'longstddecimaldegrees' and 'latstddecimaldegrees' are read-in as new site-level attributes
# ... this needs to be synced to PedonPC functions

## TODO: multiple records / site in siteobs are possible and will result in duplicate data


get_site_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	q <- "SELECT siteiid as siteiid, peiid, usiteid as site_id, upedonid as pedon_id, obsdate as obs_date, utmzone, utmeasting, utmnorthing, -(longdegrees + CASE WHEN longminutes IS NULL THEN 0.0 ELSE longminutes / 60.0 END + CASE WHEN longseconds IS NULL THEN 0.0 ELSE longseconds / 60.0 / 60.0 END) as x, latdegrees + CASE WHEN latminutes IS NULL THEN 0.0 ELSE latminutes / 60.0 END + CASE WHEN latseconds IS NULL THEN 0.0 ELSE latseconds / 60.0 / 60.0 END as y, dm.ChoiceName as datum, longstddecimaldegrees as x_std, latstddecimaldegrees as y_std, descname as describer, pp.ChoiceName as pedon_purpose, pt.ChoiceName as pedon_type, pedlabsampnum, elev as elev_field, slope as slope_field, aspect as aspect_field, plantassocnm, se.ChoiceLabel as coverkind_1, bedrckdepth, br.ChoiceLabel as bedrock_kind, bh.ChoiceLabel as bedrock_hardness, hs.ChoiceLabel as hillslope_pos, sp.ChoiceLabel as slope_position, sa.ChoiceLabel as shapeacross, sd.ChoiceLabel as shapedown, sc.ChoiceLabel as slopecomplex, dc.ChoiceLabel as drainagecl

FROM
	((((((((((((((
	
	site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref) 
	LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref) 
	LEFT OUTER JOIN (
      SELECT siteiidref, bedrckdepth, bedrckkind, bedrckhardness, ROW_NUMBER() OVER(PARTITION BY siteiidref ORDER BY bedrckdepth ASC) as rn
      FROM sitebedrock_View_1
    ) as sb ON site_View_1.siteiid = sb.siteiidref)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1261) AS dm ON site_View_1.horizdatnm = dm.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 517) AS br ON sb.bedrckkind = br.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1247) AS bh ON sb.bedrckhardness = bh.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1271) AS pp ON pedon_View_1.pedonpurpose = pp.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1273) AS pt ON pedon_View_1.pedontype = pt.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1296) AS sp ON site_View_1.geomslopeseg = sp.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 975) AS sa ON site_View_1.shapeacross = sa.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 975) AS sd ON site_View_1.shapedown = sd.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1295) AS sc ON site_View_1.slopecomplex = sc.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 148) AS dc ON site_View_1.drainagecl = dc.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 149) AS se ON siteobs_View_1.earthcovkind1 = se.ChoiceValue)
	
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 971) AS hs ON site_View_1.hillslopeprof = hs.ChoiceValue

  WHERE sb.rn IS NULL OR sb.rn = 1

	ORDER BY pedon_View_1.peiid ;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
	
	# test for no data
	if(nrow(d) == 0)
		stop('there are no pedons in your selected set!')
	
  ## TODO: this should be removed once we switch to WGS84 coordinates
	# warn if mixed datums
	if(length(unique(na.omit(d$datum))) > 1)
		message('multiple horizontal datums present, consider using WGS84 coordinates (x_std, y_std)')
	
  ## TODO: this should probably use peiid
	# are there any duplicate pedon IDs?
	t.pedon_id <- table(d$pedon_id)
	not.unique.pedon_id <- t.pedon_id > 1
	if(any(not.unique.pedon_id))
		assign('dup.pedon.ids', value=names(t.pedon_id[which(not.unique.pedon_id)]), envir=soilDB.env)
	
	# warn about sites without a matching pedon (records missing peiid)
	missing.pedon <- which(is.na(d$peiid))
	if(length(missing.pedon)> 0)
		assign('sites.missing.pedons', value=unique(d$site_id[missing.pedon]), envir=soilDB.env)
	
  # set factor levels, when it makes sense
  d$hillslope_pos <- factor(d$hillslope_pos, c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))
  
	# done
	return(d)
}

