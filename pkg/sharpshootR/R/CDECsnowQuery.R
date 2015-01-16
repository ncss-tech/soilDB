# fetch and format monthly data by course number, starting year, and ending year
CDECsnowQuery <- function(course, start_yr, end_yr) {
  # construct the URL for the DWR website	
  u <- paste(
    'http://cdec.water.ca.gov/cgi-progs/snowQuery?course_num=', course, 
    '&month=(All)&start_date=', start_yr, 
    '&end_date=', end_yr, 
    '&csv_mode=Y&data_wish=Retrieve+Data', 
    sep='')
  
  # read the result of sending the URL as a CSV file:
  # noting that it has a header,
  # skipping the first line
  # not converting characters to factors
  # interpreting '        --' as NA
  d <- read.csv(file=url(u), header=TRUE, skip=1, as.is=TRUE, na.strings='        --')
  
  if(nrow(d) == 0 | ncol(d) != 5)
    stop('query returned no data', call.=FALSE)
  
  # compute the density, as percent
  d$density <- (d$Water / d$Depth) * 100.0
  
  # add SWE collumn using Adjusted if present
  d$SWE <- ifelse(is.na(d$Adjusted), d$Water, d$Adjusted)
  
  # convert date to R-friendly format
  d$Meas.Date <- as.Date(d$Meas.Date, format="%d-%B-%Y")
  
  # convert representative date 
  # note that this month isn't the same as the month when the data were collected
  # note that we need to add an arbitrary 'day' to the string in order for it to be parsed correctly
  d$Date <- as.Date(paste('01/', d$Date, sep=''), format="%d/%m/%Y")
  
  # extract the year and month for reporting ease later
  d$year <- as.numeric(format(d$Date, "%Y"))
  d$month <- factor(format(d$Date, '%B'), levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
  
  # return the result
  return(d[, c('Meas.Date','Date','year','month','Depth','Water','Adjusted','SWE','density')])
}
