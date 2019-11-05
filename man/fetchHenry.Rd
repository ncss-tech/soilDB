\name{fetchHenry}
\alias{fetchHenry}
\alias{month2season}
\alias{summarizeSoilTemperature}
\alias{HenryTimeLine}

\title{Download Data from the Henry Mount Soil Temperature and Water Database}

\description{This function is a front-end to the REST query functionality of the Henry Mount Soil Temperature and Water Database.}

\usage{
fetchHenry(what='all', usersiteid = NULL, project = NULL, sso = NULL,
gran = "day", start.date = NULL, stop.date = NULL, 
pad.missing.days = TRUE, soiltemp.summaries = TRUE)
}

\arguments{
  \item{what}{type of data to return: 'sensors': sensor metadata only | 'soiltemp': sensor metadata + soil temperature data | 'soilVWC': sensor metadata + soil moisture data | 'airtemp': sensor metadata + air temperature data | 'waterlevel': sensor metadata + water level data |'all': sensor metadata + all sensor data}
  \item{usersiteid}{(optional) filter results using a NASIS user site ID}
  \item{project}{(optional) filter results using a project ID}
  \item{sso}{(optional) filter results using a soil survey office code}
  \item{gran}{data granularity: "day", "week", "month", "year"; returned data are averages}
  \item{start.date}{(optional) starting date filter}
  \item{stop.date}{(optional) ending date filter}
  \item{pad.missing.days}{should missing data ("day" granularity) be filled with NA? see details}
  \item{soiltemp.summaries}{should soil temperature ("day" granularity only) be summarized? see details}
}

\details{Filling missing days with NA is useful for computing and index of how complete the data are, and for estimating (mostly) unbiased MAST and seasonal mean soil temperatures. Summaries are computed by first averaging over Julian day, then averaging over all days of the year (MAST) or just those days that occur within "summer" or "winter". This approach makes it possible to estimate summaries in the presence of missing data. The quality of summaries should be weighted by the number of "functional years" (number of years with non-missing data after combining data by Julian day) and "complete years" (number of years of data with >= 365 days of non-missing data).}

\value{a list containing:
 \item{sensors}{a \code{SpatialPointsDataFrame} object containing site-level information}
 \item{soiltemp}{a \code{data.frame} object containing soil temperature timeseries data}
 \item{soilVWC}{a \code{data.frame} object containing soil moisture timeseries data}
 \item{airtemp}{a \code{data.frame} object containing air temperature timeseries data}
 \item{waterlevel}{a \code{data.frame} object containing water level timeseries data}
}

\author{D.E. Beaudette}
\note{This function and the back-end database are very much a work in progress.}

\seealso{\code{\link{fetchSCAN}}}
\examples{
\donttest{
library(lattice)

# get CA630 data as daily averages
x <- fetchHenry(project='CA630', gran = 'day')

# inspect data gaps
levelplot(factor(!is.na(sensor_value)) ~ doy * factor(year) | name, 
data=x$soiltemp, col.regions=c('grey', 'RoyalBlue'), cuts=1, 
colorkey=FALSE, as.table=TRUE, scales=list(alternating=3), 
par.strip.text=list(cex=0.75), strip=strip.custom(bg='yellow'), 
xlab='Julian Day', ylab='Year')
}
}

\keyword{manip}

