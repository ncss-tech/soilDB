\name{unroll}
\alias{unroll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unroll Genetic Horizons}
\description{
Generate a descretized vector of genetic horizons along a user-defined pattern.
}
\usage{unroll(top, bottom, prop, max_depth, bottom_padding_value = NA, strict=FALSE)}

\arguments{
  \item{top}{vector of upper horizon boundaries, must be an integer}
  \item{bottom}{vector of lower horizon boundaries, must be an integer}
  \item{prop}{vector of some property to be "unrolled" over a regular sequence}
  \item{max_depth}{maximum depth to which missing data is padded with NA}
  \item{bottom_padding_value}{value to use when padding missing data}
  \item{strict}{should horizons be strictly checked for self-consistency? defaults to FALSE}
}
\details{
This function is used internally by several higher-level components of the \code{aqp} package. Basic error checking is performed to make sure that bottom and top horizon boundaries make sense. Note that the horizons should be sorted according to depth before using this function. The \code{max_depth} argument is used to specifiy the maximum depth of profiles within a collection, so that data from any profile shallower than this depth is padded with NA.
}

\value{a vector of "unrolled" property values}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}

\examples{
data(sp1)

# subset a single soil profile:
sp1.1 <- subset(sp1, subset=id == 'P001')

# demonstrate how this function works
x <- with(sp1.1, unroll(top, bottom, prop, max_depth=50))
plot(x, 1:length(x), ylim=c(90,0), type='b', cex=0.5)	
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
