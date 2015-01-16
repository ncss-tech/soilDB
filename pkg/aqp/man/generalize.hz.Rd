\name{generalize.hz}
\alias{generalize.hz}

\title{Generalize Horizon Names}
\description{Generalize a vector of horizon names, based on new classes, and REGEX patterns.}
\usage{generalize.hz(x, new, pat, non.matching.code)}

\arguments{
  \item{x}{a character vector of horizon names}
  \item{new}{a character vector of new horizon classes}
  \item{pat}{a character vector of REGEX, same length as \code{x}}
  \item{non.matching.code}{text used to describe any horizon not matching any item in \code{pat}}
}

\value{factor of the same length as \code{x}}

\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}

\examples{
data(sp1)

# check original distribution of hz designations
table(sp1$name)

# generalize
sp1$genhz <- generalize.hz(sp1$name, 
new=c('O','A','B','C','R'), 
pat=c('O', '^A','^B','C','R'))

# see how we did / what we missed
table(sp1$genhz, sp1$name)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
