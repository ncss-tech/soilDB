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


get_site_data_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	q <- "SELECT siteiid as siteiid, peiid, CAST(usiteid AS varchar(60)) as site_id, CAST(upedonid AS varchar(60)) as pedon_id, obsdate as obs_date, utmzone, utmeasting, utmnorthing, -(longdegrees + CASE WHEN longminutes IS NULL THEN 0.0 ELSE longminutes / 60.0 END + CASE WHEN longseconds IS NULL THEN 0.0 ELSE longseconds / 60.0 / 60.0 END) as x, latdegrees + CASE WHEN latminutes IS NULL THEN 0.0 ELSE latminutes / 60.0 END + CASE WHEN latseconds IS NULL THEN 0.0 ELSE latseconds / 60.0 / 60.0 END as y, horizdatnm, longstddecimaldegrees as x_std, latstddecimaldegrees as y_std, gpspositionalerror, descname as describer, pedonpurpose, pedontype, pedlabsampnum, labdatadescflag, elev as elev_field, slope as slope_field, aspect as aspect_field, plantassocnm, siteobs_View_1.earthcovkind1, siteobs_View_1.earthcovkind2, erocl, bedrckdepth, bedrckkind, bedrckhardness, hillslopeprof, geomslopeseg, shapeacross, shapedown, slopecomplex, drainagecl, geomposhill, geomposmntn, geomposflats

FROM 

site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
LEFT OUTER JOIN 
(
      SELECT siteiidref, bedrckdepth, bedrckkind, bedrckhardness, ROW_NUMBER() OVER(PARTITION BY siteiidref ORDER BY bedrckdepth ASC) as rn
      FROM sitebedrock_View_1
) as sb ON site_View_1.siteiid = sb.siteiidref

WHERE sb.rn IS NULL OR sb.rn = 1

ORDER BY pedon_View_1.peiid ;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# toggle selected set vs. local DB
	if(SS == FALSE) {
	  q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
	}
	
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
	
	## this shouldn't happen, retain for debugging
	# test for an error
	if(class(d) == 'character')
	  stop('error in SQL')
	
	# uncode domain columns
	d <- uncode(d, stringsAsFactors = stringsAsFactors)
	
	# short-circuit: 0 rows means nothing in the selected set and thus we stop here
	if(nrow(d) == 0) {
	  return(d)
	}
	  
	
  # https://github.com/ncss-tech/soilDB/issues/41
	# warn if mixed datums
	if(length(na.omit(unique(d$horizdatnm))) > 1)
		message('multiple horizontal datums present, consider using WGS84 coordinates (x_std, y_std)')
	
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
	# most of these are done via uncode()
	
  # surface shape
  d$shapeacross <- factor(d$shapeacross, levels=c('concave', 'linear', 'convex', 'undulating', 'complex'))
  d$shapedown <- factor(d$shapedown, levels=c('concave', 'linear', 'convex', 'undulating', 'complex'))
  
  # create 3D surface shape
  d$slope_shape <- paste0(d$shapeacross, ' / ', d$shapedown)
  
  # make reasonable levels for 3D slope shape
  ss.grid <- expand.grid(na.omit(unique(d$shapeacross)), na.omit(unique(d$shapedown)))
  ss.levels <- apply(ss.grid, 1, function(i) { paste(rev(i), collapse = ' / ')})
  d$slope_shape <- factor(d$slope_shape, levels=ss.levels)
  
  # convert factors to strings
  idx <- unlist(lapply(df, is.factor))
  if (stringsAsFactors == FALSE & any(idx)) {
    df[idx] <- lapply(df[idx], as.character)
  }
  
	# done
	return(d)
}

