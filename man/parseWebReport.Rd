\name{parseWebReport}
\alias{parseWebReport}

\title{Parse contents of a web report, based on suplied arguments.}
\description{Parse contents of a web report, based on suplied arguments.}
\usage{
parseWebReport(url, args, index = 1)
}

\arguments{
  \item{url}{Base URL to a LIMS/NASIS web report.}
  \item{args}{List of named arguments to send to report, see details.}
  \item{index}{Integer index specifiying the table to rerturn, or, NULL for a list of tables}
}

\details{Report argument names can be infered by inspection of the HTML source associated with any given web report.}

\value{A \code{data.frame} object in the case of a single integer passed to \code{index}, a \code{list} object in the case of an integer vector or NULL passed to \code{index}.}

\author{D.E. Beaudette and S.M. Roecker}

\keyword{ IO }

\note{Most web reports are for internal use only.}

\examples{
# pending
}