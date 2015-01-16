\name{CDECquery}
\alias{CDECquery}

\title{Get water-related data (California only) from the CDEC website.}
\description{Get water-related data (California only) from the CDEC website.}

\usage{CDECquery(id, sensor, interval = "D", start, end)}

\arguments{
  \item{id}{station ID (e.g. 'spw'), see details}
  \item{sensor}{the sensor ID (e.g. 45), see details}
  \item{interval}{character, 'D' for daily, 'H' for hourly, 'M' for monthly}
  \item{start}{starting date, in the format 'YYYY-MM-DD'}
  \item{end}{ending date, in the format 'YYYY-MM-DD'}
}

\value{a \code{data.frame} object with the following fields: `datetime`, `year`, `month`, `value`.}
\references{\url{http://cdec.water.ca.gov/queryCSV.html}}
\author{D.E. Beaudette}

\details{
\describe{
  \item{1.}{Station IDs can be found here: \url{http://cdec.water.ca.gov/staInfo.html}}
  \item{2.}{Sensor IDs can be found using this URL \url{http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=}, followed by the station ID.}
  \item{3.}{Resevoir capacities can be found here \url{http://cdec.water.ca.gov/misc/resinfo.html}}
}
}


\seealso{
\code{\link{CDECsnowQuery}}
}

\examples{
\dontrun{
library(latticeExtra)
library(plyr)
library(e1071)

# get daily resevoir storage (ac. ft) from Pinecrest, New Melones and Lyons resevoirs
pinecrest <- CDECquery(id='swb', sensor=15, interval='D', start='2012-09-01', end='2015-01-01')
new.melones <- CDECquery(id='nml', sensor=15, interval='D', start='2012-09-01', end='2015-01-01')
lyons <- CDECquery(id='lys', sensor=15, interval='D', start='2012-09-01', end='2015-01-01')

# compute storage capacity
pinecrest$capacity <- pinecrest$value / 18312 * 100
new.melones$capacity <- new.melones$value / 2400000 * 100
lyons$capacity <- lyons$value / 6228 * 100

# combine
g <- make.groups(new.melones, lyons, pinecrest)

# resonable date scale
r <- range(g$datetime)
s.r <- seq(from=r[1], to=r[2], by='1 month')

# better colors
tps <- list(superpose.line=list(lwd=2, col=brewer.pal(n=3, name='Set1')))

# plot
xyplot(capacity ~ datetime, groups=which, data=g, type='l', 
       xlab='', ylab='Capacity (%)', ylim=c(-5, 105),
       scales=list(x=list(at=s.r, labels=format(s.r, "%b\n%Y"))), 
       auto.key=list(columns=3, lines=TRUE, points=FALSE),
       par.settings=tps,
       panel=function(...) {
         panel.abline(h=seq(0, 100, by=10), col='grey')
         panel.abline(v=s.r, col='grey')
         panel.xyplot(...)
       })


##
# New Melones monthly data, retrieve as far back in time as possible 
new.melones.monthly <- CDECquery(id='nml', sensor=15, interval='M', 
start='1900-01-01', end='2015-01-01')

# convert to pct. capacity
new.melones.monthly$capacity <- new.melones.monthly$value / 2400000 * 100


# make a nice color ramp function
cols <- colorRampPalette(brewer.pal(9, 'Spectral'), 
space='Lab', interpolate='spline')

# plot, each pixel is colored by the total precip by year/month
levelplot(capacity ~ year * month, data=new.melones.monthly, col.regions=cols, xlab='', 
ylab='', scales=list(x=list(tick.number=20)), main='New Melones Capacity (%)')


##
# get daily precip totals from Stan Powerhouse
x <- CDECquery(id='spw', sensor=45, interval='D', start='1900-01-01', end='2015-01-01')

# compute total precip by year/month
a <- ddply(x, c('year', 'month'), summarize, s=sum(value, na.rm=TRUE))

# convert monthly precipitation values into Z-scores by month
a.scaled <- ddply(a, 'month', summarize, year=year, scaled.ppt=scale(s))

# make a nice color ramp function, scaled by the skewness of the underlying distribution
cols <- colorRampPalette(brewer.pal(9, 'Spectral'), 
space='Lab', interpolate='spline', bias=skewness(a.scaled$scaled.ppt, na.rm=TRUE))

# plot, each pixel is colored by the total precip by year/month
levelplot(scaled.ppt ~ year * month, data=a.scaled, col.regions=cols, xlab='', 
ylab='', scales=list(x=list(tick.number=10)), 
main='Monthly Total Precipitation (as z-score) SPW')


##
# get pre-aggregated monthly data from Sonora RS
x <- CDECquery(id='sor', sensor=2, interval='M', start='1900-01-01', end='2015-01-01')

# make a nice color ramp function, scaled by the skewness of the underlying distribution
cols <- colorRampPalette(brewer.pal(9, 'Spectral'), space='Lab', 
interpolate='spline', bias=skewness(x$value, na.rm=TRUE))

# plot
levelplot(value ~ year * month, data=x, col.regions=cols, xlab='', 
ylab='', scales=list(x=list(tick.number=20)), main='Monthly Total Precipitation (inches) SOR')
}
}

