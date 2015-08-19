\name{fetchSCAN}
\alias{fetchSCAN}
\title{Fetch SCAN Data}
\description{Query soil/climate data from USDA-NRCS SCAN Stations (experimental)}
\usage{fetchSCAN(req)}

\arguments{
  \item{req}{a vector of named characters, e.g. 
  c(intervalType=' View Historic ', report='STO', timeseries='Daily', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')}
}
\details{An attempt is made to parse the column names from the data returned from the SCAN webservice. The data returned will depend on the report submitted. Column names contain the code (I, D, H): 

Daily sensors (e.g. TAVG.D-1) report a summary value for the previous day.  Hourly sensors (e.g. TAVG.H-1) report a summary value for the previous hour.  Instantaneous sensors (e.g. TOBS.I-1) report a single observation on the hour.

Some of the available reports include: SCAN, ALL, SOIL, SMS, STO, PREC, WEATHER, WIND, SOLAR.

See examples, details pending.}

\value{a \code{data.frame} object}
\references{http://www.wcc.nrcs.usda.gov/scan/}
\author{D.E. Beaudette}
\note{This is still an experimental function.}

\examples{
\dontrun{
# all sensors
req <- c(intervalType=' View Historic ', report='ALL', timeseries='Daily', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')

# standard SCAN report
req <- c(intervalType=' View Historic ', report='SCAN', timeseries='Daily', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')

# soil / air temps
req <- c(intervalType=' View Historic ', report='STO', timeseries='Daily', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')

# soil moisture + precip
req <- c(intervalType=' View Historic ', report='SMS', timeseries='Daily', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')

# soil moisture, hourly: careful, lots of data!
req <- c(intervalType=' View Historic ', report='SMS', timeseries='Hourly', 
format='copy', sitenum='2072', interval='YEAR', year='2011', month='CY')

# get data, format into DF
x <- fetchSCAN(req)
}
}
\keyword{manip}

