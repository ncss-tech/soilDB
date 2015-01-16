\name{aspect.plot}
\alias{aspect.plot}

\title{Plot Aspect Data}
\description{Plot a graphical summary of multiple aspect measurements on a circular diagram.}
\usage{
aspect.plot(p, q=c(0.05, 0.5, 0.95), p.bins = 60, p.bw = 30, stack=TRUE, 
p.axis = seq(0, 350, by = 10), plot.title = NULL,
line.col='RoyalBlue', line.lwd=1, line.lty=2, 
arrow.col=line.col, arrow.lwd=1, arrow.lty=1,
arrow.length=0.15,
...)
}

\arguments{
  \item{p}{a vector of aspect angles in degrees, measured clock-wise from North}
  \item{q}{a vector of desired quantiles}
  \item{p.bins}{number of bins to use for circular histogram}
  \item{p.bw}{bandwidth used for circular density estimation}
  \item{stack}{TRUE/FALSE, should the individual points be stacked into \code{p.bins} number of bins and plotted}
  \item{p.axis}{a sequence of integers (degrees) describing the circular axis}
  \item{plot.title}{an informative title}
  \item{line.col}{density line color}
  \item{line.lwd}{density line width}
  \item{line.lty}{density line line style}
  \item{arrow.col}{arrow color}
  \item{arrow.lwd}{arrow line width}
  \item{arrow.lty}{arrow line style}
  \item{arrow.length}{arrow head length}
  \item{\dots}{further arguments passed to \code{plot.circular}}
  
}
\details{Spread and central tendency are depicted with a combination of circular histogram and kernel density estimate. The circular mean, and relative confidence in that mean are depicted with an arrow: longer arrow lengths correspond to greater confidence in the mean.}

\author{D.E. Beaudette}

\note{Manual adjustment of \code{p.bw} may be required in order to get an optimal circular density plot. This function requires the package \code{circular}, version 0.4-7 or later.}

\examples{
# simulate some data
p.narrow <- runif(n=25, min=215, max=280)
p.wide <- runif(n=25, min=0, max=270)

# set figure margins to 0, 2-column plot
par(mar=c(0,0,0,0), mfcol=c(1,2))

# plot 
aspect.plot(p.narrow, p.bw=10, plot.title='Soil A', pch=21, col='black', bg='RoyalBlue')
aspect.plot(p.wide, p.bw=10, plot.title='Soil B', pch=21, col='black', bg='RoyalBlue')
}

\keyword{hplots}
