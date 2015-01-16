\name{estimateSoilDepth}
\alias{estimateSoilDepth}

\title{Estimate Soil Depth}
\description{Estimate the soil depth of a single profile within a SoilProfileCollection object.}
\usage{
estimateSoilDepth(f, name = "hzname", top = "hzdept", bottom = "hzdepb", p = 'Cr|R|Cd', 
no.contact.depth = NULL, no.contact.assigned = NULL)
}

\arguments{
  \item{f}{A SoilProfileCollection object of length 1, e.g. a single profile}
  \item{name}{the name of the column that contains horizon designations}
  \item{top}{the name of the column that contains horizon top depths}
  \item{bottom}{the name of the column that contains horizon bottom depths}
  \item{p}{a REGEX pattern for determining contact with bedrock}
  \item{no.contact.depth}{in the absense of contact with bedrock, a depth at which we can assume a standard depth}
  \item{no.contact.assigned}{assumed standard depth}
}

\value{a single integer, the soil depth}

\author{D.E. Beaudette and J.M. Skovlin}


\seealso{
\code{\link{getSoilDepthClass}}, \code{\link{profileApply}}
}
\examples{
data(sp1)
depths(sp1) <- id ~ top + bottom

# apply to each profile in a collection, and save as site-level attribute
sp1$depth <- profileApply(sp1, estimateSoilDepth, name='name', top='top', bottom='bottom')

\dontrun{
# sample data
data(gopheridge, package='soilDB')

# run on a single profile
estimateSoilDepth(gopheridge[1, ])

# apply to an entire collection
profileApply(gopheridge, estimateSoilDepth)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
