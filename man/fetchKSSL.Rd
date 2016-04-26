\name{fetchKSSL}
\alias{fetchKSSL}
\title{Fetch KSSL Data (EXPERIMENTAL)}
\description{Get soil characterization and morphologic data via BBOX, MLRA, or series name query, from the KSSL database.}
\usage{fetchKSSL(series=NULL, bbox=NULL, mlra=NULL, pedlabsampnum=NULL, 
pedon_id=NULL, pedon_key=NULL, returnMorphologicData=FALSE)}

\arguments{
  \item{series}{a soil series name, case insensitive}
  \item{bbox}{a bounding box in WGS84 geographic coordinates e.g. \code{c(-120, 37, -122, 38)}}
  \item{mlra}{an MLRA ID, e.g. "18" or "22A"}
  \item{pedlabsampnum}{KSSL pedon lab sample number}
  \item{pedon_id}{user pedon ID}
  \item{pedon_key}{KSSL internal pedon ID}
  \item{returnMorphologicData}{optionally request basic morphologic data, see details section}
}



\details{This is an experimental interface to a subset for the most commonly used data from a snapshot of KSSL (lab characterization) and NASIS (morphologic) data. The snapshots were last updated December 2015 (KSSL) and April 2016 (KSSL). 

Series-queries are case insensitive. Series name is based on the "correlated as" field (from KSSL snapshot) when present.  The "sampled as" classification was promoted to "correlated as" if the "correlated as" classification was missing.

When \code{returnMorphologicData} is TRUE, the resulting object is a list. The standard output from \code{fetchKSSL} (\code{SoilProfileCollection} object) is stored in the named element "SPC". The additional elements are basic morphologic data: horizon colors, rock fragments, pores, and structure. There is a 1:many relationship between the horizon data in "SPC" and the additional dataframes in \code{morph}. See examples for ideas on how to "flatten" these tables.
}

\value{a \code{SoilProfileCollection} object when \code{returnMorphologicData} is FALSE, otherwise a list.}

\author{D.E. Beaudette}
\note{SoilWeb maintains a snapshot of these KSSL and NASIS data. The SoilWeb snapshot was developed using methods described here: \url{https://github.com/dylanbeaudette/process-kssl-snapshot}. Please use the link below for the live data.}

\references{
\url{http://ncsslabdatamart.sc.egov.usda.gov/}
}

\seealso{\code{\link{fetchOSD}}}
\examples{
\dontrun{
# search by series name
s <- fetchKSSL(series='auburn')

# search by bounding-box
# s <- fetchKSSL(bbox=c(-120, 37, -122, 38))

# how many pedons
length(s)

# plot 
par(mar=c(0,0,0,0))
plot(s, name='hzn_desgn', max.depth=150)


# get morphologic data too
library(soilDB)
library(plyr)
library(reshape2)


# get lab and morphologic data
s <- fetchKSSL(series='auburn', returnMorphologicData = TRUE)

# extract SPC
pedons <- s$SPC

# simplify color data
s.colors <- simplifyColorData(s$morph$phcolor, id.var = 'labsampnum')

# merge color data into SPC
h <- horizons(pedons)
h <- join(h, s.colors, by='labsampnum', type='left', match='first')
horizons(pedons) <- h

# check
par(mar=c(0,0,0,0))
plot(pedons, color='moist_soil_color', print.id=FALSE)


# simplify fragment data
s.frags <- simplfyFragmentData(s$morph$phfrags, id.var='labsampnum')

# merge fragment data into SPC
h <- horizons(pedons)
h <- join(h, s.frags, by='labsampnum', type='left', match='first')
horizons(pedons) <- h


# check
par(mar=c(0,0,3,0))
plot(pedons, color='total_frags_pct', print.id=FALSE)


}
}

\keyword{utilities}
