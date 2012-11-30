# results can be referenced via phiid (horizon-level ID)
get_colors_from_NASIS_db <- function() {
	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
	q <- "SELECT dbo.pedon.peiid, phorizon.phiid as phiid, colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, colorvalue, colorchroma
FROM (
	(dbo.pedon INNER JOIN dbo.phorizon ON dbo.pedon.peiid = dbo.phorizon.peiidref)
	INNER JOIN dbo.phcolor ON dbo.phorizon.phiid = dbo.phcolor.phiidref)
	LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE DomainID = 1242) AS mh ON dbo.phcolor.colorhue = mh.ChoiceValue
	ORDER BY phiid, dbo.phcolor.colormoistst;"
  
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# convert Munsell to RGB
	cat('converting Munsell to RGB ...\n')
	d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))
	
	# re-combine
	d <- cbind(d, d.rgb)
	
	# split into dry / moist
	dry.colors <- subset(d, d$colormoistst == 1)
	moist.colors <- subset(d, d$colormoistst == 2)
	
	# mix and clean colors
	cat('mixing and cleaning colors ...\n')
	dry.colors.final <- ddply(dry.colors, 'phiid', mix_and_clean_colors, .progress='text')
	moist.colors.final <- ddply(moist.colors, 'phiid', mix_and_clean_colors, .progress='text')
	
	# rename columns
	names(dry.colors.final) <- c('phiid', 'd_r', 'd_g', 'd_b', 'd_value')
	names(moist.colors.final) <- c('phiid', 'm_r', 'm_g', 'm_b', 'm_value')
	
	# merge into single df
	d.final <- join(dry.colors.final, moist.colors.final, by='phiid', type='full')
	
	# clean-up
	rm(d, d.rgb, dry.colors, moist.colors, dry.colors.final, moist.colors.final)
	gc()
	
	# done
	return(d.final)
}

