\name{sp4}
\alias{sp4}
\docType{data}
\title{Soil Chemical Data from Serpentinitic Soils of California}
\description{Soil Chemical Data from Serpentinitic Soils of California}
\usage{data(sp4)}
\format{
  A data frame with 30 observations on the following 13 variables.
  \describe{
    \item{\code{id}}{site name}
    \item{\code{name}}{horizon designation}
    \item{\code{top}}{horizon top boundary in cm}
    \item{\code{bottom}}{horizon bottom boundary in cm}
    \item{\code{K}}{exchangeable K in c mol/kg}
    \item{\code{Mg}}{exchangeable Mg in cmol/kg}
    \item{\code{Ca}}{exchangeable Ca in cmol/kg}
    \item{\code{CEC_7}}{cation exchange capacity (NH4OAc at pH 7)}
    \item{\code{ex_Ca_to_Mg}}{extractable Ca:Mg ratio}
    \item{\code{sand}}{sand content by weight percentage}
    \item{\code{silt}}{silt content by weight percentage}
    \item{\code{clay}}{clay content by weight percentage}
    \item{\code{CF}}{>2mm fraction by volume percentage}
  }
}
\details{Selected soil physical and chemical data from (McGahan et al., 2009).}
\source{https://www.soils.org/publications/sssaj/articles/73/6/2087}
\references{McGahan, D.G., Southard, R.J, Claassen, V.P. 2009. 
Plant-Available Calcium Varies Widely in Soils on Serpentinite Landscapes. 
Soil Sci. Soc. Am. J. 73: 2087-2095.}
\examples{
# setup environment
library(aqp)

# load sample data set, a simple data.frame object with horizon-level data from 10 profiles
data(sp4)
str(sp4)

# optionally read about it...
# ?sp4

# upgrade to SoilProfileCollection
# 'id' is the name of the column containing the profile ID
# 'top' is the name of the column containing horizon upper boundaries
# 'bottom' is the name of the column containing horizon lower boundaries
depths(sp4) <- id ~ top + bottom

# check it out
class(sp4) # class name
str(sp4) # internal structure

# inspect object properties
idname(sp4) # self-explanitory
horizonDepths(sp4) # self-explanitory

# you can change these:
depth_units(sp4) # defaults to 'cm'
metadata(sp4) # not much to start with

# alter the depth unit metadata
depth_units(sp4) <- 'inches' # units are really 'cm'

# more generic interface for adjusting metadata
md <- metadata(sp4) # save original metadata

# add columns
md$describer <- 'DGM'
md$date <- as.Date('2009-01-01')
md$citation <- 'McGahan, D.G., Southard, R.J, Claassen, V.P. 
2009. Plant-Available Calcium Varies Widely in Soils 
on Serpentinite Landscapes. Soil Sci. Soc. Am. J. 73: 2087-2095.'

# re-assign
metadata(sp4) <- md
depth_units(sp4) <- 'cm' # fix depth units, back to 'cm'

# further inspection with common function overloads
length(sp4) # number of profiles in the collection
nrow(sp4) # number of horizons in the collection
names(sp4) # column names
min(sp4) # shallowest profile depth in collection
max(sp4) # deepest profile depth in collection

# extraction of soil profile components
profile_id(sp4) # vector of profile IDs
horizons(sp4) # horizon data

# extraction of specific horizon attributes
sp4$clay # vector of clay content

# subsetting SoilProfileCollection objects
sp4[1, ] # first profile in the collection
sp4[, 1] # first horizon from each profile

# basic plot method, highly customizable: see manual page ?plotSPC
plot(sp4)
# inspect plotting area, very simple to overlay graphical elements
abline(v=1:length(sp4), lty=3, col='blue')
# profiles are centered at integers, from 1 to length(obj)
axis(1, line=-1.5, at=1:10, cex.axis=0.75, font=4, col='blue', lwd=2)
# y-axis is based on profile depths
axis(2, line=-1, at=pretty(1:max(sp4)), cex.axis=0.75, font=4, las=1, col='blue', lwd=2)


# symbolize soil properties via color
par(mar=c(0,0,4,0))
plot(sp4, color='clay')
plot(sp4, color='CF')

# apply a function to each profile, returning a single value per profile,
# in the same order as profile_id(sp4)
soil.depths <- profileApply(sp4, max) # recall that max() gives the depth of a soil profile

# check that the order is correct
all.equal(names(soil.depths), profile_id(sp4))

# a vector of values that is the same length as the number of profiles
# can be stored into site-level data
sp4$depth <- soil.depths
# check: looks good
max(sp4[1, ]) == sp4$depth[1]

# extract site-level data 
site(sp4) # as a data.frame
sp4$depth # specific columns as a vector

# use site-level data to alter plotting order
new.order <- order(sp4$depth) # the result is an index of rank
par(mar=c(0,0,0,0))
plot(sp4, plot.order=new.order)

# deconstruct SoilProfileCollection into a data.frame, with horizon+site data
as(sp4, 'data.frame')
}
\keyword{datasets}
