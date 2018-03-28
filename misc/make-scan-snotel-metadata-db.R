library(soilDB)
library(plyr)

# https://github.com/ncss-tech/soilDB/issues/61

# attempt to cross-reference a lab ID via pedon ID
# using a LIMS report and HTML scraping, ick
# about 5 seconds per request
getLabPedon <- function(pedonID) {
  url <- sprintf('https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon+Description+html+(userpedid)&pedon_id=%s', pedonID)
  
  rpt <- read_html(url)
  n <- html_node(rpt, xpath = "//*/table/tr[10]/td[1]/*")
  n <- xml_text(n)
  lab.id <- gsub(' ', '', strsplit(n, ':')[[1]][2])
  
  return(lab.id)
}

getLabPedon <- Vectorize(getLabPedon)



##
## station list / site information
##

# get these data from SCAN/SNOTEL www map, zoom all the way out and then click on export to CSV
# there are some trash data in here, trailing tabs
x <- read.csv('scan-snotel-data/station-metadata.csv', stringsAsFactors = FALSE, colClasses = 'character')

# fix formatting
x$Name <- trimws(x$Name)
x$ID <- as.numeric(trimws(x$ID))
x$State <- trimws(x$State)
x$Network <- trimws(x$Network)
x$County <- trimws(x$County)
x$Elevation_ft <- as.numeric(trimws(x$Elevation_ft))
x$Latitude <- as.numeric(trimws(x$Latitude))
x$Longitude <- as.numeric(trimws(x$Longitude))
x$HUC <- trimws(x$HUC)

# re-name ID
names(x)[2] <- 'Site'

# check: OK
nrow(x)
str(x)


##
## pedon / lab IDs 
##

## SCAN / SNOTEL sites from western US
# most of these files are maintained by regional staff
# naming convention from NASIS site table
p.west <- read.csv('scan-snotel-data/Utah_DCO_Soil_Lab_Data.csv', stringsAsFactors = FALSE)

# whats in here:
# many sites from several states! 
str(p.west)
table(p.west$state)

# keep subset of columns
p.west <- p.west[, c('climstaid', 'climstanm', 'upedonid', 'pedlabsampnum')]

# re-name ID
names(p.west)[1] <- 'Site'

# re-name for mixing
names(p.west)[-1] <- paste0(names(p.west)[-1], '-WEST')

# check: ok
str(p.west)



## SCAN data via Steve Campbell / soil climate center
# missing lab IDs
# missing SNOTEL sites
p.scan <- read.csv('scan-snotel-data/SCAN_Pedon_Master.csv', stringsAsFactors = FALSE)

str(p.scan)
table(p.scan$State)

# re-name to match other metadata
names(p.scan) <- c('Site', 'climstanm', 'state', 'upedonid')

# look-up lab ID via LIMS report
# takes a couple of minutes
# some pedon IDs won't map to a lab ID (not linked in NASIS)
p.scan$pedlabsampnum <- getLabPedon(p.scan$upedonid)

# replace missing values with NA
p.scan$pedlabsampnum[which(p.scan$pedlabsampnum == '')] <- NA

# re-name and subset columns
p.scan <- p.scan[, c('Site', 'climstanm', 'upedonid', 'pedlabsampnum')]
names(p.scan)[-1] <- paste0(names(p.scan)[-1], '-SCAN')


##
## merge metada from various sources, filling in the missing values with best available data
##

# unique set of site IDs
m <- data.frame(Site=unique(c(p.west$Site, p.scan$Site)), stringsAsFactors = FALSE)

# western data
m <- join(m, p.west, by='Site', type='left')

# SCAN master list
m <- join(m, p.scan, by='Site', type='left')

# new columns for best-available
m$climstanm <- NA
m$upedonid <- NA
m$pedlabsampnum <- NA

### TODO: double check logic
# select best available
m$climstanm <- ifelse(! is.na(m$`climstanm-WEST`), m$`climstanm-WEST`, m$`climstanm-SCAN`)
m$upedonid <- ifelse(! is.na(m$`upedonid-WEST`), m$`upedonid-WEST`, m$`upedonid-SCAN`)
m$pedlabsampnum <- ifelse(! is.na(m$`pedlabsampnum-WEST`), m$`pedlabsampnum-WEST`, m$`pedlabsampnum-SCAN`)


##
## combine site metadata and pedon links
##

SCAN_SNOTEL_metadata <- join(x, m[, c('Site', 'climstanm', 'upedonid', 'pedlabsampnum')], by='Site', type='left')

# check for possible errors via station name comparison
idx <- which( ! SCAN_SNOTEL_metadata$Name == SCAN_SNOTEL_metadata$climstanm )
SCAN_SNOTEL_metadata[idx, c('Site', 'Name', 'climstanm')]

# hmm... mostly abbreviations and spelling

# save as R data file
save(SCAN_SNOTEL_metadata, file='../data/SCAN_SNOTEL_metadata.rda')

