# https://github.com/ncss-tech/soilDB/issues/61

## get these data from SCAN www map, zoom all the way out and then click on export to CSV

x <- read.csv('scan-snotel-metadata.csv', stringsAsFactors = FALSE)

# fix HUC formatting
x$HUC <- as.character(as.numeric(x$HUC))

# check: OK
str(x)

# save as R data file
SCAN_SNOTEL_metadata <- x
save(SCAN_SNOTEL_metadata, file='../data/SCAN_SNOTEL_metadata.rda')

