\name{test_hz_logic}
\alias{test_hz_logic}

\title{Test Horizon Logic}
\description{Simple tests for horizon logic, based on a simple data.frame of ordered horizons.}
\usage{
test_hz_logic(i, topcol, bottomcol, strict = FALSE)
}
\arguments{
  \item{i}{a data.frame associated with a single soil profile, ordered by depth}
  \item{topcol}{character, giving the name of the column in \code{i} that describes horizon top depth}
  \item{bottomcol}{character, giving the name of the column in \code{i} that describes horizon bottom depth}
  \item{strict}{logical, should continuity tests be performed-- i.e. for non-contiguous horizon boundaries}
}
\details{By default, this function tests for NA and overlapping horizons. If any either are encountered, FALSE is returned.}
\value{logical: TRUE --> pass, FALSE --> fail}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{D.E. Beaudette}

\seealso{\link{depths<-}}
\examples{
## simple example: just one profile
data(sp1)
depths(sp1) <- id ~ top + bottom
s <- horizons(sp1[1, ])

## check
# fails due to missing hz boundary
s$bottom[6] <- NA # missing horizon boundary, common on bottom-most hz
test_hz_logic(s, 'top', 'bottom', strict=FALSE)

# fails due to inconsistent hz boundary
s$bottom[3] <- 30 # inconsistent hz boundary
test_hz_logic(s, 'top', 'bottom', strict=TRUE)


## filtering bad data 
\dontrun{
# missing bottom horizons
x$hzn_bot[!is.na(x$hzn_top) & is.na(x$hzn_bot)] <- x$hzn_top[!is.na(x$hzn_top) & is.na(x$hzn_bot)]

# remove O horizons where top > bottom
bad.O.hz.idx <- which(x$hzn_top > x$hzn_bot)
if(length(bad.O.hz.idx) > 0)
	x <- x[-bad.O.hz.idx, ]
}

## checking for bad data: do this before promoting to SoilProfileCollection object
library(plyr)
data(sp1)

# horizon logic can be tested via data.frame, at 2 levels of scrutiny:
ddply(sp1, 'id', test_hz_logic, topcol='top', bottomcol='bottom', strict=FALSE)
ddply(sp1, 'id', test_hz_logic, topcol='top', bottomcol='bottom', strict=TRUE)
}

\keyword{manip}

