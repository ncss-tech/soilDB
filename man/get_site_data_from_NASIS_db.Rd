\name{get_site_data_from_NASIS_db}
\alias{get_site_data_from_NASIS_db}

\title{Extract Site Data from a local NASIS Database}
\description{Get site-level data from a local NASIS database.}
\usage{get_site_data_from_NASIS_db()}

\value{A dataframe.}
\author{Jay M. Skovlin and Dylan E. Beaudette}
\note{This function currently works only on Windows.}

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, 
}

\examples{

\dontrun{

## Example: export / convert DMS coordinates from NASIS and save to DD import file

# load required libraries
library(soilDB)
library(rgdal)
library(plyr)

# get site data from NASIS
s <- get_site_data_from_NASIS_db()

# keep only those pedons with real coordinates
good.idx <- which(!is.na(s$x))
s <- s[good.idx, ]

# investigate multiple datums:
table(s$datum, useNA='always')

## this is not universally appropriate!
# assume missing is NAD83 
s$datum[is.na(s$datum)] <- 'NAD83'

# check: OK
table(s$datum, useNA='always')

# convert to NAD83
old.coords <- cbind(s$x, s$y)

# add temp colum for projection information, and fill with proj4 style info
s$proj4 <- rep(NA, times=nrow(s))
s$proj4 <- paste('+proj=longlat +datum=', s$datum, sep='')

# iterate over pedons, and convert to WGS84
new.coords <- ddply(s, 'peiid', 
										.progress='text', .fun=function(i) {
											coordinates(i) <- ~ x + y
											proj4string(i) <- CRS(i$proj4)
											i.t <- spTransform(i, CRS('+proj=longlat +datum=WGS84'))
											i.c <- as.matrix(coordinates(i.t))
											return(data.frame(x.new=i.c[, 1], y.new=i.c[, 2]))
										})

# merge in new coordinates
s <- join(s, new.coords)

# any changes?
summary(sqrt(apply((s[, c('x', 'y')] - s[, c('x.new', 'y.new')])^2, 1, sum)))

# save to update file for use with "Import of Standard WGS84 Georeference" calculation in NASIS
# note that this defines the coordinate source as "GPS", hence the last column of '1's.
std.coordinates.update.data <- unique(cbind(s[, c('siteiid', 'y.new', 'x.new')], 1))
write.table(std.coordinates.update.data, file='c:/data/sgeoref.txt', col.names=FALSE, row.names=FALSE, sep='|')
}}

\keyword{manip}

