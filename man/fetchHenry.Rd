\name{fetchHenry}
\alias{fetchHenry}
\title{Download Data from the Henry Mount Soil Climate Database (experimental)}
\description{This function is a front-end to the REST query functionality of the Henry Mount Soil Climate Database.}
\usage{
fetchHenry(usersiteid = NULL, project = NULL, type = "soiltemp", gran = "day", 
start.date = NULL, stop.date = NULL, 
pad.missing.days = TRUE, soiltemp.summaries = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usersiteid}{(optional) filter results using a NASIS user site ID}
  \item{project}{(optional) filter results using a project ID}
  \item{type}{sensor type, currently only "soiltemp" is supported}
  \item{gran}{data granularity: "day", "week", "month", "year"; returned data are averages}
  \item{start.date}{(optional) starting date filter}
  \item{stop.date}{(optional) ending date filter}
  \item{pad.missing.days}{should missing data ("day" granularity) be filled with NA? see details}
  \item{soiltemp.summaries}{should soil temperature ("day" granularity only) be summarized? see details}
}

\details{Filling missing days with NA is useful for computing and index of how complete the data are, and for estimating (mostly) unbiased MAST and seasonal mean soil temperatures. Summaries are computed by first averaging over Julian day, then averaging over all days of the year (MAST) or just those days that occur within "summer" or "winter". This approach makes it possible to estimate summaries in the presence of missing data. The quality of summaries should be weighted by the number of "functional years" (days of data / 365.25) and "complete years" (number of years of data with >= 365 days of non-missing data).}

\value{a list containing:
 \item{sites}{a \code{SpatialPointsDataFrame} object containing site-level information}
 \item{soiltemp}{a \code{data.frame} object containing soil temperature timeseries data}
}

\author{D.E. Beaudette}
\note{This function and the back-end database are very much a work in progress.}

\seealso{\code{\link{fetchSCAN}}}
\examples{
\dontrun{
library(lattice)

# get CA630 data as daily averages
x <- fetchHenry(project='CA630', gran = 'day')

# inspect data gaps
levelplot(factor(!is.na(sensor_value)) ~ doy * factor(year) | id, 
data=x$soiltemp, col.regions=c('grey', 'RoyalBlue'), cuts=1, 
colorkey=FALSE, as.table=TRUE, scales=list(alternating=3), 
par.strip.text=list(cex=0.75), strip=strip.custom(bg='yellow'), 
xlab='Julian Day', ylab='Year')
}
}

\keyword{manip}

