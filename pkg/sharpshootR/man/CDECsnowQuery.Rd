\name{CDECsnowQuery}
\alias{CDECsnowQuery}

\title{Get snow survey data (California only) from the CDEC website.}
\description{Get snow survey data (California only) from the CDEC website.}

\usage{
CDECsnowQuery(course, start_yr, end_yr)
}

\arguments{
  \item{course}{integer, course number (e.g. 129)}
  \item{start_yr}{integer, the starting year (e.g. 2010)}
  \item{end_yr}{integer, the ending year (e.g. 2013)}
}

\details{This function downloads data from the CDEC website, therefore an internet connection is required. The `SWE` column contains adjusted SWE if available (`Adjusted` column), otherwise the reported SWE is used (`Water` column).}
\value{a \code{data.frame} object, see examples}

\references{\url{http://cdec.water.ca.gov/cgi-progs/snowQuery}}

\author{D.E. Beaudette}

\note{Snow course locations, ID numbers, and other information can be found here: \url{http://cdec.water.ca.gov/misc/SnowCourses.html}}

\examples{
\dontrun{
# get data for course numbe 129
x <- CDECsnowQuery(course=129, start_yr=2010, end_yr=2011)
}
}
