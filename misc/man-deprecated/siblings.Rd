\name{siblings}
\alias{siblings}

\title{Lookup siblings and cousins for a given soil series.}
\description{Lookup siblings and cousins for a given soil series, from the current fiscal year SSURGO snapshot via SoilWeb.}
\usage{
siblings(s, only.major=FALSE, component.data = FALSE, cousins = FALSE)
}

\arguments{
  \item{s}{character vector, the name of a single soil series, case-insensitive.}
  \item{only.major}{logical, should only return siblings that are major components}
  \item{component.data}{logical, should component data for siblings (and optionally cousins) be returned?}
  \item{cousins}{logical, should siblings-of-siblings (cousins) be returned?}
}

\details{The siblings of any given soil series are defined as those soil series (major and minor component) that share a parent map unit with the named series (as a major component). Cousins are siblings of siblings. Data are sourced from SoilWeb which maintains a copy of the current SSURGO snapshot.}

\value{
\describe{
	\item{sib}{\code{data.frame} containing siblings, major component flag, and number of co-occurrences}
	\item{sib.data}{\code{data.frame} containing sibling component data}
	\item{cousins}{\code{data.frame} containing cousins, major component flag, and number of co-occurrences}
	\item{cousin.data}{\code{data.frame} containing cousin component data}
	} 
}

\references{
\href{http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html}{soilDB Soil Series Query Functionality}

\href{http://ncss-tech.github.io/AQP/soilDB/siblings.html}{Related tutorial.}
}

\author{
D.E. Beaudette
}

\seealso{
\link{OSDquery}, \link{siblings}, \link{fetchOSD}
}

\examples{
\donttest{
if(requireNamespace("curl") &
    curl::has_internet()) {
    
    # basic usage
    x <- siblings('zook')
    x$sib
    
    # restrict to siblings that are major components
    # e.g. the most likely siblings
    x <- siblings('zook', only.major = TRUE)
    x$sib
}
}
}

\keyword{ manip }

