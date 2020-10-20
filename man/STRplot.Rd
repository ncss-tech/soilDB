\name{STRplot}
\alias{STRplot}

\title{Graphical Description of US Soil Taxonomy Soil Temperature Regimes}
\description{Graphical Description of US Soil Taxonomy Soil Temperature Regimes}

\usage{
STRplot(mast, msst, mwst, permafrost = FALSE, pt.cex = 2.75, leg.cex = 0.85)
}

\arguments{
  \item{mast}{single value or vector of mean annual soil temperature (deg C)}
  \item{msst}{single value or vector of mean summer soil temperature (deg C)}
  \item{mwst}{single value of mean winter soil temperature (deg C)}
  \item{permafrost}{logical: permafrost presence / absence}
  \item{pt.cex}{symbol size}
  \item{leg.cex}{legend size}
}

\details{
\href{http://ncss-tech.github.io/AQP/soilDB/STR-eval.html}{Related tutorial}.
}

\references{
Soil Survey Staff. 2015. Illustrated guide to soil taxonomy. U.S. Department of Agriculture, Natural Resources Conservation Service, National Soil Survey Center, Lincoln, Nebraska.
}

\author{D.E. Beaudette}


\seealso{
\code{\link{estimateSTR}}
}

\examples{
par(mar=c(4,1,0,1))
STRplot(mast = 0:25, msst = 10, mwst = 1)
}

\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")

