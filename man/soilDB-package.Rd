\name{soilDB-package}
\alias{soilDB-package}
\alias{soilDB}
\docType{package}
\title{Soil Database Interface}
\description{This package provides methods for extracting soils information from local PedonPC and AK Site databases (MS Access format), local NASIS databases (MS SQL Server), and the SDA webservice. Currently USDA-NCSS data sources are supported, however, there are plans to develop interfaces to outside systems such as the Global Soil Mapping project.}
\details{It can be difficult to locate all of the dependencies required for sending/processing SOAP requests, especially on UNIX-like operating systems. Windows binary packages for the dependencies can be found here \url{http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.14/}. }
\author{
D.E. Beaudette and J.M. Skovlin
Maintainer: D.E. Beaudette <dylan.beaudette@ca.usda.gov>
}
\references{}
\keyword{package}
\seealso{}
\examples{
\dontrun{
##
## PedonPC Examples
##

# 1. load data in pieces
dsn <- "S:/Service_Center/NRCS/pedon/pedon.mdb"
site_data <- get_site_data_from_pedon_db(dsn)
hz_data <- get_hz_data_from_pedon_db(dsn)
color_data <- get_colors_from_pedon_db(dsn)

# 2. join pieces
# horizon + hz color: all horizons
h <- join(hz_data, color_data, by='phiid', type='left')

# (hz + color) + site: only those with horizon data
f <- join(h, site_data, by='peiid', type='inner')


# 3. fix some common problems
# replace missing lower boundaries
f$hzdepb[!is.na(f$hzdept) & is.na(f$hzdepb)] <- f$hzdept[!is.na(f$hzdept) & is.na(f$hzdepb)]

# test for bad horizonation... flag, and remove
f.test <- ddply(f, .(pedon_id), test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)

# which are the good (valid) ones?
good.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == TRUE)])

# keep the good ones
f <- subset(f, pedon_id \%in\% good.pedon.ids)

# 4. upgrade to SoilProfilecollection
depths(f) <- pedon_id ~ hzdept + hzdepb

# move site data into @site
site(f) <- ~ sampled_as + correlated_as + hillslope_pos + x + y + datum + elev + slope + aspect + plantassocnm + bedrckdepth + bedrock_kind + describer + psctopdepth + pscbotdepth + part_size_class

# 5. convert colors... in the presence of missing color data
f$soil_color <- rep(NA, times=nrow(horizons(f)))
idx <- complete.cases(f$m_r)
f$soil_color[idx] <- with(horizons(f)[idx, ], rgb(m_r, m_g, m_b)) # moist colors


##
## NASIS examples
##
## these depend on the user setting up a new DSN within the ODBC Data Sources Control Panel
## and naming it 'nasis_local'
## see instructions from P. Finnel
##

# 1. fetch data from Local NASIS DB
dsn <- 'nasis_local'
site_data <- get_site_data_from_NASIS_db(dsn)
hz_data <- get_hz_data_from_NASIS_db(dsn)
color_data <- get_colors_from_NASIS_db(dsn)

# 2. join data into single object
h <- join(hz_data, color_data, by='phiid', type='left')
f <- join(h, site_data, by='peiid', type='inner')

# see pedonPC examples for further processing


}	
}
