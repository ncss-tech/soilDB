\name{addVolumeFraction}
\alias{addVolumeFraction}
\title{Symbolize Volume Fraction on a Soil Profile Collection Plot}
\description{Symbolize volume fraction on an existing soil profile collection plot.}

\usage{
addVolumeFraction(x, colname, res = 10, cex.min = 0.1, 
cex.max = 0.5, pch = 1, col = "black")
}

\arguments{
  \item{x}{a \code{SoilProfileCollection} object}
  \item{colname}{character vector of length 1, naming the column containing volume fraction data (horizon-level attribute)}
  \item{res}{integer, resolution of the grid used to symbolize volume fraction}
  \item{cex.min}{minimum symbol size}
  \item{cex.max}{maximum symbol size}
  \item{pch}{plotting character}
  \item{col}{color of the symbol}
}

\details{This function can only be called after plotting a \code{SoilProfileCollection} object.}
\note{Details associated with a call to plot.SoilProfileCollection are automatically accounted for within this function: e.g. \code{plot.order}, \code{width}, etc.}

\author{D.E. Beaudette}

\seealso{
\code{\link{plotSPC}}
}

\examples{
# sample data
data(loafercreek, package='soilDB')

# subset first 10 profiles 
s <- loafercreek[1:10, ]

# replace 0% frags with NA
s$total_frags_pct[which(s$total_frags_pct == 0)] <- NA

# plot in random order, note that annotations follow
par(mar=c(0, 0, 3, 0))
plot(s, color='total_frags_pct', plot.order=sample(1:length(s)))
addVolumeFraction(s, 'total_frags_pct', pch=1)

par(mar=c(0, 0, 0, 0))
plot(s, max.depth=100, name='total_frags_pct', cex.name=1, axis.line.offset=-4.25)
addVolumeFraction(s, 'total_frags_pct', pch=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
