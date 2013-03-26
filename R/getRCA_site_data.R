## TODO: spatial data will likely be referenced to multiple datums... 
# the STD coordiants in NASIS are WGS84, but have to be manually "calculated"
# see: Import of Standard WGS84 Georeference
# for now, 'longstddecimaldegrees' and 'latstddecimaldegrees' are read-in as new site-level attributes
# ... this needs to be synced to PedonPC functions

# TODO: sitebedrock _may_ contain more than 1 row / site... this will result in duplicate rows returned by this function

getRCA_site_data <- function() {
	q <- "SELECT siteiid as siteiid, peiid, usiteid as site_id, upedonid as pedon_id, rcasiteid, pc.ChoiceLabel as plotconfig, exposedsoilpct, rcapointnumber, azimuthfromplotcenter, distancefromplotcenter, ba.ChoiceLabel as basalareafactor, numberoftreesin, localdisturbancedistance, localdisturbancedescription, drainedflag, beddingflag, plantationflag, fr.ChoiceLabel as forestrotationstage, 

obsdate as obs_date, longstddecimaldegrees as x_std, latstddecimaldegrees as y_std, pp.ChoiceLabel as pedon_purpose, pt.ChoiceLabel as pedon_type, pedlabsampnum, psctopdepth, pscbotdepth, elev as elev_field, slope as slope_field, aspect as aspect_field, plantassocnm, bedrckdepth, br.ChoiceLabel as bedrock_kind, bh.ChoiceLabel as bedrock_hardness, hs.ChoiceLabel as hillslope_pos, sp.ChoiceLabel as slope_position, sa.ChoiceLabel as shapeacross, sd.ChoiceLabel as shapedown, sc.ChoiceLabel as slopecomplex, dc.ChoiceLabel as drainagecl
FROM
	((((((((((((((((
	
	site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref) 
LEFT OUTER JOIN sitewoodybasalarea_View_1 ON sitewoodybasalarea_View_1.siteobsiidref = siteobs_View_1.siteobsiid) 
LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref) 
LEFT OUTER JOIN sitebedrock_View_1 ON site_View_1.siteiid = sitebedrock_View_1.siteiidref)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 517) AS br ON sitebedrock_View_1.bedrckkind = br.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1247) AS bh ON sitebedrock_View_1.bedrckhardness = bh.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1271) AS pp ON pedon_View_1.pedonpurpose = pp.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1273) AS pt ON pedon_View_1.pedontype = pt.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4938) AS pc ON site_View_1.sampleplotconfiguration = pc.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4927) AS ba ON sitewoodybasalarea_View_1.basalareafactor = ba.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4925) AS fr ON siteobs_View_1.forestrotationstage = fr.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1296) AS sp ON site_View_1.geomslopeseg = sp.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 975) AS sa ON site_View_1.shapeacross = sa.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 975) AS sd ON site_View_1.shapedown = sd.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1295) AS sc ON site_View_1.slopecomplex = sc.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 148) AS dc ON site_View_1.drainagecl = dc.ChoiceValue)

LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 971) AS hs ON site_View_1.hillslopeprof = hs.ChoiceValue
ORDER BY pedon_View_1.peiid ;"
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# test for no data
	if(nrow(d) == 0)
		stop('there are no pedons in your selected set!')
	
	# are there any duplicate pedon IDs?
	t.pedon_id <- table(d$pedon_id)
	not.unique.pedon_id <- t.pedon_id > 1
	if(any(not.unique.pedon_id)) {
		message('NOTICE: duplicate pedons:', appendLF=TRUE)
		print(t.pedon_id[which(not.unique.pedon_id)])
	}
	
	# warn about sites without a matching pedon (records missing peiid)
	missing.pedon <- which(is.na(d$peiid))
	if(length(missing.pedon)> 0) {
		message(paste('NOTICE: sites without pedons:', paste(unique(d$site_id[missing.pedon]), collapse=', ')))
	}
	
	# done
	return(d)
}

