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

## TODO: bug within RODBC - converts site_id == 056E916010 to an exponent


get_site_data_from_NASIS_db_new <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	q <- "SELECT siteiid as siteiid, peiid, CAST(usiteid AS varchar(60)) as site_id, CAST(upedonid AS varchar(60)) as pedon_id, obsdate as obs_date, utmzone, utmeasting, utmnorthing, -(longdegrees + CASE WHEN longminutes IS NULL THEN 0.0 ELSE longminutes / 60.0 END + CASE WHEN longseconds IS NULL THEN 0.0 ELSE longseconds / 60.0 / 60.0 END) as x, latdegrees + CASE WHEN latminutes IS NULL THEN 0.0 ELSE latminutes / 60.0 END + CASE WHEN latseconds IS NULL THEN 0.0 ELSE latseconds / 60.0 / 60.0 END as y, horizdatnm, longstddecimaldegrees as x_std, latstddecimaldegrees as y_std, gpspositionalerror, descname as describer, pedonpurpose, pedontype, pedlabsampnum, labdatadescflag, elev as elev_field, slope as slope_field, aspect as aspect_field, plantassocnm, siteobs_View_1.earthcovkind1, pedon_View_1.earthcovkind1, erocl, bedrckdepth, bedrckkind, bedrckhardness, hillslopeprof, geomslopeseg, shapeacross, shapedown, slopecomplex, drainagecl, geomposhill, geomposmntn, geomposflats

FROM site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
	LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
	LEFT OUTER JOIN (
      SELECT siteiidref, bedrckdepth, bedrckkind, bedrckhardness, ROW_NUMBER() OVER(PARTITION BY siteiidref ORDER BY bedrckdepth ASC) as rn
      FROM sitebedrock_View_1
    ) as sb ON site_View_1.siteiid = sb.siteiidref

  WHERE sb.rn IS NULL OR sb.rn = 1

	ORDER BY pedon_View_1.peiid ;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
	
	# test for an error
	if(class(d) == 'character')
	  stop('error in SQL', call. = FALSE)
	
	# test for no data
	if(nrow(d) == 0)
		stop('there are no pedons in your selected set!', call. = FALSE)
	
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
	
  ## set factor levels, when it makes sense
	# hill slope position
  d$hillslopepos <- factor(d$hillslopepos, c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))
  
  # surface shape
  d$shapeacross <- factor(d$shapeacross, levels=c('Concave', 'Linear', 'Convex'))
  d$shapedown <- factor(d$shapedown, levels=c('Concave', 'Linear', 'Convex'))
  
  # create 3D surface shape
  d$slope_shape <- paste0(d$shapeacross, ' / ', d$shapedown)
  # make reasonable levels for 3D slope shape
  ss.grid <- expand.grid(levels(d$shapeacross), levels(d$shapedown))
  ss.levels <- apply(ss.grid, 1, function(i) { paste(rev(i), collapse = ' / ')})
  d$slopeshape <- factor(d$slopeshape, levels=ss.levels)
  
  # geomcomponent, hills
  d$geomposhill <- factor(d$geomposhill, levels=c('Base Slope', 'Head Slope', 'Side Slope', 'Nose Slope', 'Crest', 'Interfluve', 'Free Face'))
  
  # geomcomponent, mountains
  d$geomposmntn <- factor(d$geomposmntn, levels=c('Mountainbase', 'Lower third of mountainflank', 'Center third of mountainflank', 'Mountainflank', 'Upper third of mountainflank', 'Mountaintop', 'Free face'))
  
  # geomcomponent, flats
  d$geomposflats <- factor(d$geomposflats, levels=c('Dip', 'Talf', 'Rise'))
  
  # drainage class
  d$drainagecl <- factor(d$drainagecl, levels=c("Very poorly drained", "Poorly drained", "Somewhat poorly drained", "Moderately well drained", "Well drained", "Somewhat excessively drained", "Excessively drained"))
  
  # uncode domain columns
  d <- uncode(d)
  
	# done
	return(d)
}

