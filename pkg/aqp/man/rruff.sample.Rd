\name{rruff.sample}
\alias{rruff.sample}
\docType{data}
\title{
Sample XRD Patterns
}
\description{
Several sample XRD patterns from the RRUFF project site.
}
\usage{data(rruff.sample)}
\format{
  A data frame with 3000 observations on the following 8 variables.
  \describe{
    \item{\code{twotheta}}{twotheta values}
    \item{\code{nontronite}}{XRD pattern for nontronite}
    \item{\code{montmorillonite}}{XRD pattern for montmorillonite}
    \item{\code{clinochlore}}{XRD pattern for clinochlore}
    \item{\code{antigorite}}{XRD pattern for antigorite}
    \item{\code{chamosite}}{XRD pattern for chamosite}
    \item{\code{hematite}}{XRD pattern for hematite}
\item{\code{goethite}}{XRD pattern for goethite}
  }
}

\source{
http://rruff.info/
}
\references{
http://rruff.info/
}
\examples{
data(rruff.sample)

# plot all patterns
matplot(rruff.sample, type='l', lty=1)
}
\keyword{datasets}
