get_colors_from_pedon_db <-
function(dsn)
  {
  # color data... check
  q <- "SELECT phorizon.phiid, colormoistst, colorpct as pct, mh.choice AS colorhue, colorvalue, colorchroma
FROM (
(pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref)
INNER JOIN phcolor ON phorizon.phiid = phcolor.phiidref)
LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1242) AS mh ON phcolor.colorhue = mh.choice_id
ORDER BY phiidref, colormoistst;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
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
  dry.colors.final <- ddply(dry.colors, c('phiid'), mix_and_clean_colors, .progress='text')
  moist.colors.final <- ddply(moist.colors, c('phiid'), mix_and_clean_colors, .progress='text')

  # rename columns
  names(dry.colors.final) <- c('phiid','d_r','d_g','d_b')
  names(moist.colors.final) <- c('phiid','m_r','m_g','m_b')

  # merge into single df
  d.final <- join(dry.colors.final, moist.colors.final, type='full')
  
  # clean-up
  rm(d, d.rgb, dry.colors, moist.colors, dry.colors.final, moist.colors.final)
  gc()
  
  # done
  return(d.final)
  }

