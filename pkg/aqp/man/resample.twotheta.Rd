\name{resample.twotheta}
\alias{resample.twotheta}
\title{
Resample an XRD Pattern
}
\description{
Resample an XRD pattern along a user-defined twotheta resolution via local spline interpolation.
}
\usage{
resample.twotheta(twotheta, x, tt.min = min(twotheta), 
tt.max = max(twotheta), new.res = 0.02)
}
\arguments{
  \item{twotheta}{a vector of twotheta value}
  \item{x}{a vector of diffraction intensities corresponding with twotheta values}
  \item{tt.min}{new minimum twotheta value, defaults to current minimum}
  \item{tt.max}{new maximum twotheta value, defaults to current maximum}
  \item{new.res}{new twotheta resolution, defaults to 0.02}
}
\details{Sometimes XRD patterns are collected at different resolutions, or at a resolution that is too great for full pattern matching. This function can be used to resample patterns to a consistent twotheta resolution, or to decimate massive patterns.}
\value{
A dataframe with the following columns
	\item{twotheta}{new sequence of twotheta values}
	\item{x}{resampled diffraction intensities}
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E Beaudette}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{rruff.sample}}}

\examples{
data(rruff.sample)

# resample single pattern
nontronite.resamp <- with(rruff.sample, 
resample.twotheta(twotheta, nontronite, new.res=0.02) )

# plot original vs. resampled pattern
plot(nontronite ~ twotheta, data=rruff.sample, type='l', col='grey')
lines(nontronite.resamp, col='blue')

}
\keyword{manip}

