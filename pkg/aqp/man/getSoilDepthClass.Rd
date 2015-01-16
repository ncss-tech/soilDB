\name{getSoilDepthClass}
\alias{getSoilDepthClass}
\title{Generate Soil Depth Class Matrix}
\description{Generate a boolean matrix of soil depth classes from a SoilProfileCollection object.}
\usage{
getSoilDepthClass(f, depth.classes = c(very.shallow = 25, 
shallow = 50, mod.deep = 100, deep = 150, very.deep = 1000), ...)
}

\arguments{
  \item{f}{a SoilProfileCollection object}
  \item{depth.classes}{a named vector of classes and depth breaks}
  \item{\dots}{arguments passed to \code{\link{estimateSoilDepth}}}
}

\value{a \code{data.frame} containing soil depth and depth class for each profile, see examples}

\author{D.E. Beaudette and J.M. Skovlin}


\seealso{
\code{\link{estimateSoilDepth}}
}

\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# generate depth-class matrix
sdc <- getSoilDepthClass(sp1, name='name', top='top', bottom='bottom')

# inspect
head(sdc)

# join back into sp1 as site-level data
site(sp1) <- sdc

\dontrun{
# sample data
data(gopheridge, package='soilDB')

getSoilDepthClass(gopheridge)
}
}

\keyword{manip}

