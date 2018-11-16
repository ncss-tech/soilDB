\name{siblings}
\alias{siblings}

\title{Lookup siblings and cousins for a given soil series.}
\description{Lookup siblings and cousins for a given soil series, from the current fiscal year SSURGO snapshot via SoilWeb.}
\usage{
siblings(s, component.data = FALSE, cousins = FALSE)
}

\arguments{
  \item{s}{character vector, the name of a single soil series, case-insensitive.}
  \item{component.data}{logical, should component data for siblings (and optionally cousins) be returned?}
  \item{cousins}{logical, should siblings-of-siblings (cousins) be returned?}
}

\details{The siblings of any given soil series are defined as those soil series that share a parent map unit. Cousins are siblings of siblings. Data are sourced from SoilWeb which maintains a copy of the current SSURGO snapshot.}

\value{
\describe{
	\item{sib}{\code{data.frame} containing siblings and number of co-occurrences}
	\item{sib.data}{\code{data.frame} containing sibling component data}
	\item{cousins}{\code{data.frame} containing cousins and number of co-occurrences}
	\item{cousin.data}{\code{data.frame} containing cousin component data}
	} 
}

\references{
\href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{soilDB Soil Series Query Functionality}
}

\author{
D.E. Beaudette
}

\seealso{
\link{OSDquery}, \link{siblings}, \link{fetchOSD}
}

\examples{
x <- siblings('zook')
str(x, 1)
}

\keyword{ manip }

