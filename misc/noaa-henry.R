# Plot Henry waterlevel data with precipitation data from nearest NOAA weather station
#
# @purpose: Show use of two new experimental soilDB functions for the NOAA API. You will need a recent version of soilDB off of GitHub to use this functionality.
#
#            - get_NOAA_stations_nearXY() - find all stations near a specified lat/lng and bounding box (limit 1000)
#
#            - get_NOAA_GHCND() - get GHCND data (daily summaries) by station ID(s), years(s) and datatypeid(s)
#
#           Note that using the NOAA API requires an API token specified as argument to above two functions.
#           A token can be obtained for free at the following link: https://www.ncdc.noaa.gov/cdo-web/token
#
# @last_update: 2020/06/18
#
# @authors: Andrew Brown, Dylan Beaudette
#
#           based on fetchHenry/fetchSCAN "Water Level and Precipitation" demo by Dylan E. Beaudette
#           (http://ncss-tech.github.io/AQP/soilDB/Henry-demo.html)

##### SETUP #####

# 1.  You will need your own API Token: https://www.ncdc.noaa.gov/cdo-web/token
noaa_api_token <- "yourtokenhere"

# # 2. You will need a Henry project/siteid/sso + water level sensor name + type

# this example is one of ben marshall's waterlevel sensors in Maryland
henry_project <- "MD021" # modify the fetchHenry call to use usersiteid, sso, etc.
henry_sensor_name <- "Hatboro"
henry_sensor_type <- "waterlevel"

# this parameter is used to convert to UTM for distance calculation in meters
utm_proj4 <- "+proj=utm +zone=18" 
use_metric <- TRUE # distance/depth/rainfall in km/cm or mi/in?

#################

# data wrangling
library(sp)
library(dplyr)

# API queries to HENRY and NOAA GHCND
library(soilDB)

# plots
library(latticeExtra)

# get data from henry 
x <- fetchHenry(
    project = henry_project, 
    what = 'all',
    gran = 'day',
    pad.missing.days = FALSE
  )

# subset potential multi-sensor result by sensor type (waterlevel) and sensor name
x.sub <- subset(x[[henry_sensor_type]], sensor_name == henry_sensor_name)

# convert Henry date/time into Date class
x.sub$date_time <- as.Date(x.sub$date_time)

# extract Henry coordinates using sensor/site name 
henry_coords <- data.frame(id = henry_sensor_name, 
                           coordinates(x$sensors[x$sensors$name == henry_sensor_name, ]))

# promote to SpatialPointsDataFrame
coordinates(henry_coords) <- ~ wgs84_longitude + wgs84_latitude
proj4string(henry_coords) <- "+proj=longlat +datum=WGS84"

# download all the stations within a half-degree lat/lng of the henry coordinates
#  using get_NOAA_stations_nearXY()
stations <- get_NOAA_stations_nearXY(
    lat = henry_coords$wgs84_latitude,
    lng = henry_coords$wgs84_longitude,
    apitoken = noaa_api_token
  )

# remove stations with less than 90 percent data coverage
# and make sure they have data at least more recent than 2010
stations <-  filter(stations,
                    datacoverage >= 0.90,
                    stations$maxdate > as.Date("2010-01-01"))

# promote point locations to SpatialPointsDataFrame
stations.sp <- stations[, c("id", "longitude", "latitude")]
coordinates(stations.sp) <- ~ longitude + latitude
proj4string(stations.sp) <- "+proj=longlat +datum=WGS84"

stations.sp <- spTransform(stations.sp, CRS(utm_proj4))
henry_coords <- spTransform(henry_coords, CRS(utm_proj4))

# calculate spatial distance between all stations and the coordinates of Henry site
dmat <- sp::spDistsN1(stations.sp, henry_coords)
stations$distance_km <- dmat / 1000

# determine the 10 nearest stations (could also set a distance threshold -- in degrees)
idx.nearest <- order(dmat)[1:10]
noaa.stations <- stations[idx.nearest, ]

# create a date range based on the Henry data
#   using the limits of the water level data and pad 14 days
start.date <- min(x.sub$date_time) - 14
stop.date <- max(x.sub$date_time) + 14

# make date axis for graph
date.axis <- seq.Date(start.date, stop.date, by = '2 months')

# filter to get NOAA stations that have data [mindate, maxdate]
#  within the HENRY interval [start.date, stop.date]
noaa.stations.inrange <- filter(noaa.stations, 
                                mindate <= start.date, 
                                maxdate >= stop.date)

# the first row is the closest station with data coverage for full interval of Henry data
noaa.station <- noaa.stations.inrange[1, ]

# determine what years to download precipitation data for
#  based on the henry start and stop dates
first.year <- as.numeric(min(format(as.Date(date.axis), "%Y")))
last.year <- as.numeric(max(format(as.Date(date.axis), "%Y")))
year.seq <- as.character(first.year:last.year)

# now, loop through each year and download the GHCND data (daily summaries)
res <- get_NOAA_GHCND(stations = noaa.station$id, years = year.seq, 
                      datatypeid = "PRCP", apitoken = noaa_api_token)

# filter result to get JUST precipitation data
res.precip <- filter(res, datatype == "PRCP")

# convert 10ths of millimeters (integer storage of decimal) to centimeters
res.precip$value <- res.precip$value / 100

ylabel1 <- 'Water Level (cm)'
ylabel2 <- 'Precipitation (cm)'

# if use_metric == FALSE, convert precip and water level from cm to inches
if(!use_metric) {
  res.precip$value <- res.precip$value / 2.54
  x.sub$sensor_value <- x.sub$sensor_value / 2.54
  
  ylabel1 <- 'Water Level (in)'
  ylabel2 <- 'Precipitation (in)'
}

# convert date to Date object for plotting
res.precip$date <- as.Date(res.precip$date)

# plot water level data, save to object
p.1 <- xyplot(sensor_value ~ date_time,
    data = x.sub,
    type = c('l', 'g'),
    cex = 0.75,
    ylab = ylabel1,
    xlab = '',
    scales = list(
      x = list(at = date.axis, format = "%b\n%Y"),
      y = list(tick.number = 10)
    )
  )

# plot precip data, save to object
#  using date.axis, so even if the data are outside range, plot will line up
p.2 <- xyplot(value ~ date,
              data = res.precip,
              as.table = TRUE,
              type = c('h'),
              strip = strip.custom(bg = grey(0.80)),
              scales = list(x = list(at = date.axis, format = "%b\n%Y")),
              ylab = ylabel2)

# combine plots into panels (latticeExtra feature)
p.3 <- c(p.1, p.2, layout = c(1, 2), x.same = TRUE)

# calculate distance value to show
distshow <- ifelse(use_metric, 
                         paste(round(noaa.station$distance_km, 1), 'km'),
                         paste(round(noaa.station$distance_km / 1.609, 1), 'mi'))

# make combined plot
update(p.3,
  scales = list(alternating = 3, y = list(rot = 0)),
  ylab = c(ylabel1, ylabel2),
  main = sprintf('Water Level (HENRY: %s)\nPrecipitation (NOAA: %s)\nDistance: %s',
                henry_sensor_name, noaa.station$name, distshow),
  xlim = c(start.date, stop.date),
  panel = function(...) {
    panel.xyplot(...)
    panel.abline(v = date.axis, col = 'grey', lty = 3)
    panel.grid(h = -1,
               v = 0,
               col = 'grey',
               lty = 3)
  }
)

