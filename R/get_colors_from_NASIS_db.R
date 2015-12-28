## 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon
## consider mixing and returning closest Munsell color... does this make sense in RGB color space?
## consider getting only the most common color?
## consider returning the most common hue?

# results can be referenced via phiid (horizon-level ID)
get_colors_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
	q <- "SELECT peiid, phiid, colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, colorvalue, colorchroma
FROM
pedon_View_1 INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
	INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS mh ON phcolor_View_1.colorhue = mh.ChoiceValue
	ORDER BY phiid, phcolor_View_1.colormoistst;"
  
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
  RODBC::odbcClose(channel)
	
	# convert Munsell to RGB
	d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))
	
	# re-combine
	d <- cbind(d, d.rgb)
  
  # add a fake column for storing `sigma`
  # this is the error associated with the rgb -> munsell transformation
  d$sigma <- NA
  
	# split into dry / moist
	dry.colors <- d[which(d$colormoistst == 1), ]
	moist.colors <- d[which(d$colormoistst == 2), ]
	
  ## there may be cases where there are 0 records of dry or moist colors
  
	# split-out those data that need color mixing:
	dry.to.mix <- names(which(table(dry.colors$phiid) > 1))
	moist.to.mix <- names(which(table(moist.colors$phiid) > 1))
  
  # names of those columns to retain
  vars.to.keep <- c("phiid", "r", "g", "b", "colorhue", "colorvalue", "colorchroma", 'sigma')
  
	# mix/combine if there are any horizons that need mixing
	if(length(dry.to.mix) > 0) {
		message(paste('mixing dry colors ... [', length(dry.to.mix), ' of ', nrow(dry.colors), ' horizons]', sep=''))
		
		# filter out and mix only colors with >1 color / horizon
		dry.mix.idx <- which(dry.colors$phiid %in% dry.to.mix)
		mixed.dry <- ddply(dry.colors[dry.mix.idx, ], 'phiid', mix_and_clean_colors)
		# combine original[-horizons to be mixed] + mixed horizons
		dry.colors.final <- rbind(dry.colors[-dry.mix.idx, vars.to.keep], mixed.dry)
		names(dry.colors.final) <- c('phiid', 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
	}
	else {# otherwise subset the columns only
		dry.colors.final <- dry.colors[, vars.to.keep]
		names(dry.colors.final) <- c('phiid', 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
	}
	
	# mix/combine if there are any horizons that need mixing
	if(length(moist.to.mix) > 0) {
	  message(paste('mixing moist colors ... [', length(moist.to.mix), ' of ', nrow(moist.colors), ' horizons]', sep=''))
		
		# filter out and mix only colors with >1 color / horizon
		moist.mix.idx <- which(moist.colors$phiid %in% moist.to.mix)
		mixed.moist <- ddply(moist.colors[moist.mix.idx, ], 'phiid', mix_and_clean_colors)
		# combine original[-horizons to be mixed] + mixed horizons
		moist.colors.final <- rbind(moist.colors[-moist.mix.idx, vars.to.keep], mixed.moist)
		names(moist.colors.final) <- c('phiid', 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
	}
	else {# otherwise subset the columns only
		moist.colors.final <- moist.colors[, vars.to.keep]
		names(moist.colors.final) <- c('phiid', 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
	}
	
	# merge into single df
	d.final <- join(dry.colors.final, moist.colors.final, by='phiid', type='full')
	
	# clean-up
	rm(d, d.rgb, dry.colors, moist.colors, dry.colors.final, moist.colors.final)
	gc()
	
	# done
	return(d.final)
}

