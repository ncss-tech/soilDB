\name{waterDayYear}
\alias{waterDayYear}

\title{Compute Water Day and Year}
\description{Compute "water" day and year, based on the end of the typical or legal dry season. This is September 30 in California.}

\usage{
waterDayYear(d, end = "09-30")
}

\arguments{
  \item{d}{anything the can be safely converted to \code{PPOSIXlt}}
  \item{end}{"MM-DD" notation for end of water year}
}

\details{This function doesn't know about leap-years. Probably worth checking.}

\value{
A \code{data.frame} object with the following
  \item{wy}{the "water year"}
  \item{wd}{the "water day"}
}

\references{Ideas borrowed from:
\url{https://github.com/USGS-R/dataRetrieval/issues/246} and
\url{https://stackoverflow.com/questions/48123049/create-day-index-based-on-water-year}
}

\author{D.E. Beaudette}


\examples{
# try it
waterDayYear('2019-01-01')
}

\keyword{ manip }% use one of  RShowDoc("KEYWORDS")

