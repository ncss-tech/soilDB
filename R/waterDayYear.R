
## TODO: leap years? 365 vs 366 total days

# compute water year and day
# d: anythihng the can be safely converted it POSIXlt
# end: MM-DD notation for end of water year
waterDayYear <- function(d, end="09-30") {
  # convert to water year, using Sept
  # ideas from: https://github.com/USGS-R/dataRetrieval/issues/246
  dLT <- as.POSIXlt(d)
  
  # add century from epoch
  water_year <- dLT$year + 1900
  
  # water year starts in September of prior year
  water_year[which(dLT$mon >= 9)] <- water_year[which(dLT$mon >= 9)]+1
  
  # convert to integer
  
  
  # dates of prior water year start
  lastWY <- as.Date(sprintf("%s-%s", water_year - 1, end))
  
  # compute water "day" as days since start of water year
  # ideas from: https://stackoverflow.com/questions/48123049/create-day-index-based-on-water-year
  water_day <- as.integer(difftime(d, lastWY, units = 'days'))
  
  return(data.frame(wy=water_year, wd=water_day))
}

