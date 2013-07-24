# 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon

# results can be referenced via phiid (horizon-level ID)
get_colors_from_NASIS_db <- function() {
	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
	q <- "SELECT peiid, phiid, colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, colorvalue, colorchroma
FROM ((
pedon_View_1 INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref)
	INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref)
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS mh ON phcolor_View_1.colorhue = mh.ChoiceValue
	ORDER BY phiid, phcolor_View_1.colormoistst;"
  
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# convert Munsell to RGB
	message('converting Munsell to RGB ...')
	d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))
	
	# re-combine
	d <- cbind(d, d.rgb)
	
	# split into dry / moist
	dry.colors <- d[which(d$colormoistst == 1), ]
	moist.colors <- d[which(d$colormoistst == 2), ]
	
	# split-out those data that need color mixing:
	dry.to.mix <- names(which(table(dry.colors$phiid) > 1))
	moist.to.mix <- names(which(table(moist.colors$phiid) > 1))
	
	# mix/combine if there are any horizons that need mixing
	if(length(dry.to.mix) > 0) {
		message(paste('mixing multiple colors ... [', length(dry.to.mix), ' horizons]', sep=''))
		
		# filter out and mix only colors with >1 color / horizon
		dry.mix.idx <- which(dry.colors$phiid %in% dry.to.mix)
		mixed.dry <- ddply(dry.colors[dry.mix.idx, ], 'phiid', mix_and_clean_colors)
		# combine original[-horizons to be mixed] + mixed horizons
		dry.colors.final <- rbind(dry.colors[-dry.mix.idx, c("phiid", "r", "g", "b", "colorvalue")], mixed.dry)
	}
	else # otherwise subset the columns only
		dry.colors.final <- dry.colors[, c("phiid", "r", "g", "b", "colorvalue")]
	
	# mix/combine if there are any horizons that need mixing
	if(length(moist.to.mix) > 0) {
		message(paste('mixing multiple colors ... [', length(moist.to.mix), ' horizons]', sep=''))
		
		# filter out and mix only colors with >1 color / horizon
		moist.mix.idx <- which(moist.colors$phiid %in% moist.to.mix)
		mixed.moist <- ddply(moist.colors[moist.mix.idx, ], 'phiid', mix_and_clean_colors)
		# combine original[-horizons to be mixed] + mixed horizons
		moist.colors.final <- rbind(moist.colors[-moist.mix.idx, c("phiid", "r", "g", "b", "colorvalue")], mixed.moist)
	}
	else # otherwise subset the columns only
		moist.colors.final <- moist.colors[, c("phiid", "r", "g", "b", "colorvalue")]
	
	
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

