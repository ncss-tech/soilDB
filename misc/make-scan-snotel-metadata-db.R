library(plyr)

# https://github.com/ncss-tech/soilDB/issues/61

## get these data from SCAN www map, zoom all the way out and then click on export to CSV
x <- read.csv('scan-snotel-data/station-metadata.csv', stringsAsFactors = FALSE)

# fix HUC formatting
x$HUC <- as.character(as.numeric(x$HUC))

# re-name ID
names(x)[2] <- 'Site'

# check: OK
str(x)


## most of these files are maintained by regional staff
## naming convention from NASIS site table
p <- read.csv('scan-snotel-data/Utah_DCO_Soil_Lab_Data.csv', stringsAsFactors = FALSE)

# whats in here:
# many sites from several states! 
str(p)
table(p$state)

# keep subset of columns
p <- p[, c('climstaid', 'climstanm', 'upedonid', 'pedlabsampnum')]

# re-name ID
names(p)[1] <- 'Site'

# check: ok
str(p)


## combine site metadata and pedon links
SCAN_SNOTEL_metadata <- join(x, p, by='Site', type='left')

# check for possible errors via station name comparison
idx <- which( ! SCAN_SNOTEL_metadata$Name == SCAN_SNOTEL_metadata$climstanm )
SCAN_SNOTEL_metadata[idx, c('Site', 'Name', 'climstanm')]

# hmm... mostly abbreviations and spelling

# save as R data file
save(SCAN_SNOTEL_metadata, file='../data/SCAN_SNOTEL_metadata.rda')

