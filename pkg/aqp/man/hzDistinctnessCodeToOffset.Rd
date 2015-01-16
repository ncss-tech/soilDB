\name{hzDistinctnessCodeToOffset}
\alias{hzDistinctnessCodeToOffset}

\title{Convert Horizon Distinctness Codes}
\description{This function accepts a vector of horizon distinctness codes, a look-up vector of codes, and a corresponding vector of vertical offsets. The defaults are based on USDA-NCSS field methods.}

\usage{
hzDistinctnessCodeToOffset(x, codes = c("A", "C", "G", "D"), offset = c(0.5, 1.5, 5, 10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of distinctness codes to be converted to offsets}
  \item{codes}{vector of unique distinctness codes}
  \item{offset}{vector of offsets associated with distinctness codes}
}
\details{Missing data (NA) or codes that are not defined in the `codes' argument are returned as 0 offsets.}
\value{a vector of vertical offsets, with the same length as the number of distinctness codes passed to the function}
\references{\url{http://soils.usda.gov/technical/fieldbook/}}
\author{D.E. Beaudette}

\seealso{\code{\link{plotSPC}}}
\examples{
data(sp1)
hzDistinctnessCodeToOffset(sp1$bound_distinct)
}

\keyword{manip}

