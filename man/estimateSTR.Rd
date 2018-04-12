\name{estimateSTR}
\alias{estimateSTR}

\title{Estimate Soil Temperature Regime}
\description{Estimate soil temperature regime (STR) based on mean annual soil temperature (MAST), mean summer temperature (MSST), mean winter soil temperature (MWST), presence of O horizons, saturated conditions, and presence of permafrost. Several assumptions are made when O horizon  or saturation are undefined.}

\usage{
estimateSTR(mast, mean.summer, mean.winter, O.hz = NA, saturated = NA, permafrost = FALSE)
}

\arguments{
  \item{mast}{vector of mean annual soil temperature (deg C)}
  \item{mean.summer}{vector of mean summer soil temperature (deg C)}
  \item{mean.winter}{vector of mean winter soil temperature (deg C)}
  \item{O.hz}{logical vector of O horizon presence / absense}
  \item{saturated}{logical vector of seasonal saturation}
  \item{permafrost}{logical vector of permafrost presence / absense}
}

\details{Pending.

\href{http://ncss-tech.github.io/AQP/soilDB/STR-eval.html}{Related tutorial}.
}

\value{Vector of soil temperature regimes.}

\references{
Soil Survey Staff. 2015. Illustrated guide to soil taxonomy. U.S. Department of Agriculture, Natural Resources Conservation Service, National Soil Survey Center, Lincoln, Nebraska.
}

\author{D.E. Beaudette}


\seealso{
\code{\link{STRplot}}
}

\examples{
# simple example
estimateSTR(mast=17, mean.summer = 22, mean.winter = 12)

}

\keyword{ manip }% use one of  RShowDoc("KEYWORDS")

