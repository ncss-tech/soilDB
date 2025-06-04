## Coordinate the latest SCAN/SNOTEL/SNOLITE metadata from authoritative sources.
## J.M. Skovlin and D.E. Beaudette
## 2024-09-17

library(soilDB)
library(jsonlite)


## Notes
#  * closes https://github.com/ncss-tech/soilDB/issues/61
#  * still a manual process that requires progressive updates in NASIS, LDM, and WCIS
#  * 


## WCIS notes:
# * pedonCode is pedonkey
# * stationID is Site

## us state names and abbreviations
s <- data.frame(
    abbr = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", 
             "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", 
             "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", 
             "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", 
             "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", 
             "WY"), 
    name = c("Alaska", "Alabama", "Arkansas", "Arizona", "California", 
             "Colorado", "Connecticut", "District of Columbia", "Delaware", 
             "Florida", "Georgia", "Hawaii", "Iowa", "Idaho", "Illinois", 
             "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", 
             "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi", 
             "Montana", "North Carolina", "North Dakota", "Nebraska", "New Hampshire", 
             "New Jersey", "New Mexico", "Nevada", "New York", "Ohio", "Oklahoma", 
             "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
             "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Vermont", 
             "Washington", "Wisconsin", "West Virginia", "Wyoming")
)




## get the latest station metadata from WCIS database API
x <- fromJSON('https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/stations?activeOnly=true&returnForecastPointMetadata=false&returnReservoirMetadata=false&returnStationElements=false&stationTriplets=*%3A*%3ASNTL%2C*%3A*%3ASCAN%2C*%3A*%3ASNTLT%2C*%3A*%3ACSCAN')

str(x)

# note the new timezone field!
# helps with https://github.com/ncss-tech/soilDB/issues/184 
table(x$dataTimeZone)


## check for errors in the WCIS records

# remove any pedon keys with alpha-numeric characters
idx <- grep('[A-Z]', x$pedonCode)

# flag these for corrections (thanks Jay!)
x[idx, ]

# remove for now
x <- x[-idx, ]

# 1186
nrow(x)


## get corresponding user Pedon ID and user Site ID from NASIS via SDA-LDM
pedon.keys <- format_SQL_in_statement(na.omit(as.integer(x$pedonCode)))


sql <- sprintf("
SELECT pedon_key, upedonid, pedlabsampnum
FROM
lab_combine_nasis_ncss AS l
WHERE l.pedon_key IN %s ;", pedon.keys)

d <- SDA_query(sql)

# 389 rows
nrow(d)

head(d)

# ensure no missing pedon_key
# should not be possible given SQL, but still..
anyNA(d$pedon_key)


## combine
x <- merge(x, d, by.x = 'pedonCode', by.y = 'pedon_key', sort = FALSE, all.x = TRUE)

head(x)


# ensure no duplication
stopifnot(! any(table(x$stationId) > 1))



## launder / correlate column names so as not to break existing code and documentation
nm <- names(x)

# normalize to pedon_key as used in LDM
names(x)[which(nm == 'pedonCode')] <- 'pedon_key'

# stationID -> Site
names(x)[which(nm == 'stationId')] <- 'Site'

# state name from abbreviation
names(x)[which(nm == 'stationId')] <- 'Site'
x$State <- s$name[match(x$stateCode, s$abbr)]

# station name
names(x)[which(nm == 'name')] <- 'Name'

# networkCode
names(x)[which(nm == 'networkCode')] <- 'Network'

# HUC
names(x)[which(nm == 'huc')] <- 'HUC'

# elevation -> Elevation_ft
names(x)[which(nm == 'elevation')] <- 'Elevation_ft'

# coordinates
names(x)[which(nm == 'latitude')] <- 'Latitude'
names(x)[which(nm == 'longitude')] <- 'Longitude'


## TODO after the next iteration
## adjust names
# nm <- c("pedon_key", "stationTriplet", "Site", "stateCode", "Network", 
#   "Name", "dcoCode", "countyName", "HUC", "Elevation_ft", "Latitude", 
#   "Longitude", "dataTimeZone", "shefId", "beginDate", "endDate", 
#   "upedonid", "pedlabsampnum", "State")


# 
# ## compare with old metadata: compare / contrast
# m <- SCAN_site_metadata()
# nrow(m)
# 
# head(m)
# 
# # note old column names, we need to preserve these
# c("Name", "Site", "State", "Network", "County", "Elevation_ft", 
#   "Latitude", "Longitude", "HUC", "climstanm", "upedonid", "pedlabsampnum")
# 
# 
# 
# # 262 populated pedlabsampnum
# table(! is.na(m$pedlabsampnum))
# 
# # 390 populated pedlabsampnum
# table(! is.na(x$pedlabsampnum))
# 
# # 262 populated upedonid
# table(! is.na(m$upedonid))
# 
# # 390 populated upedonid
# table(! is.na(x$upedonid))
# 


## make permanent
write.csv(x, "data-raw/scan-snotel-current/station-metadata.csv", row.names = FALSE)

SCAN_SNOTEL_metadata <- x
usethis::use_data(SCAN_SNOTEL_metadata, compress = "xz")
