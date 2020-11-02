library(aqp)
library(soilDB)
library(httr)
library(jsonlite)


x <- data.frame(
  sand = 33,
  silt = 33,
  clay = 33
)

ROSETTA(x, v = "1")
ROSETTA(x, v = "3")


# bogus request
x <- data.frame(
  sand = 33,
  silt = 33,
  clay = 55
)

ROSETTA(x)


# try using class centroids
x <- texcl_to_ssc(SoilTextureLevels())

ROSETTA(x)
ROSETTA(x, conf = httr::verbose())



## stress test

qq <- "SELECT TOP 50000 compname, hzname, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r FROM component AS co JOIN chorizon AS ch ON co.cokey = ch.cokey WHERE claytotal_r IS NOT NULL ;"

s <- SDA_query(qq)

# filter NA
s <- na.omit(s)

str(s)

# ~ 40k records: 35 seconds
# JSON parsing error due to NaN: illegal combination of values?
system.time(r <- ROSETTA(s[, c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r')]))

str(r)
