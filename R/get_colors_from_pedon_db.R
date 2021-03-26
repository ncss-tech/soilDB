# 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon



#' Extract Soil Color Data from a PedonPC Database
#' 
#' Get, format, mix, and return color data from a PedonPC database.
#' 
#' This function currently works only on Windows.
#' 
#' @param dsn The path to a 'pedon.mdb' database.
#' @return A data.frame with the results.
#' @author Dylan E. Beaudette and Jay M. Skovlin
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_site_data_from_pedon_db}}
#' @keywords manip
#' @export get_colors_from_pedon_db
get_colors_from_pedon_db <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
	# color data... check
	q <- "SELECT phorizon.phiid as phiid, colormoistst, colorpct as pct, colorhue, colorvalue, colorchroma
FROM (
	(pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref)
	INNER JOIN phcolor ON phorizon.phiid = phcolor.phiidref)
	ORDER BY phorizon.phiid, colormoistst;"
  
	# setup connection to our pedon database
	channel <- DBI::dbConnect(drv = odbc::odbc(), 
	                     .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; Dbq=", dsn))
	
	# exec query
	d <- DBI::dbGetQuery(channel, q)
	
	# close connection
	DBI::dbDisconnect(channel)

	# uncode domained columns
	d <- uncode(d)
	
	# convert Munsell to RGB
	cat('converting Munsell to RGB ...\n')
	d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))
	
	# re-combine
	d <- cbind(d, d.rgb)
	
	# split into dry / moist
	dry.colors <- d[which(d$colormoistst == 1), ]
	moist.colors <- d[which(d$colormoistst == 2), ]
	
	# mix and clean colors
	cat('mixing and cleaning colors ...\n')
	
	# split-out those data that need color mixing:
	dry.to.mix <- names(which(table(dry.colors$phiid) > 1))
	moist.to.mix <- names(which(table(moist.colors$phiid) > 1))
	
	# mix/combine if there are any horizons that need mixing
	if(length(dry.to.mix) > 0) {
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

