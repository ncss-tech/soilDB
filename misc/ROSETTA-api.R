library(aqp)
library(soilDB)
library(httr)
library(jsonlite)


# lots of data, some with missing values
q <- "SELECT TOP 10 musym, co.cokey, compname, comppct_r,
    hzname, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r,
    wthirdbar_r/100 AS wthirdbar_decimal, wfifteenbar_r/100 AS wfifteenbar_decimal
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    INNER JOIN component co ON mu.mukey = co.mukey
    INNER JOIN chorizon ch ON co.cokey = ch.cokey
    WHERE legend.areasymbol LIKE 'TX%'
    ORDER BY musym, co.cokey, ch.hzdept_r ASC;"

x <- SDA_query(q)

x

# sprinkle NA
x$dbthirdbar_r[1] <- NA
x$wthirdbar_decimal[2] <- NA
x$wfifteenbar_decimal[3] <- NA
x$sandtotal_r[9] <- NA
x[10, ] <- NA

# attempting to use all possible soil properties
vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')


rr <- ROSETTA(x, vars = vars)

# missing 1/3 bar Db -> model 2
rr$.rosetta.model[1] == 2

# missing 1/3 bar WT -> model 3
rr$.rosetta.model[2] == 3

# missing 15 bar WT -> model 4
rr$.rosetta.model[3] == 4

# no records missing -> model 5
all(rr$.rosetta.model[4:8] == 5)

# missing sand -> NA
is.na(rr$theta_r[9]) & (rr$.rosetta.model[9] == -1)

# all NA
all(is.na(rr[10, c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')]))




x <- data.frame(
  sand = 33,
  silt = 33,
  clay = 33
)

ROSETTA(x, vars = c('sand', 'silt', 'clay'), v = "1")

ROSETTA(x, vars = c('sand', 'silt', 'clay'), v = "3")


# bogus request
x <- data.frame(
  sand = 33,
  silt = 33,
  clay = 55
)

ROSETTA(x, vars = c('sand', 'silt', 'clay'))


# try using class centroids
x <- texcl_to_ssc(SoilTextureLevels())

ROSETTA(x, vars = c('sand', 'silt', 'clay'))
ROSETTA(x, vars = c('sand', 'silt', 'clay'), chunkSize = 10)


ROSETTA(x, vars = c('sand', 'silt', 'clay'), conf = httr::verbose())



## stress test

# lots of data, some with missing values
qq <- "SELECT TOP 50000 
compname, hzname, hzdept_r, hzdepb_r, 
sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r, wthirdbar_r/100 AS wthirdbar_decimal, wfifteenbar_r/100 AS wfifteenbar_decimal 
FROM component AS co 
LEFT JOIN chorizon AS ch ON co.cokey = ch.cokey ;"

s <- SDA_query(qq)

str(s)


vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')

# automatic model selection: 50k records: ~ 43 seconds
system.time(r <- ROSETTA(s, vars = vars, m = "0"))

str(r)
summary(r)



# ~ 84k records
# lots of data, some with missing values
# asking for more than this may cause problems
q <- "SELECT 
musym, co.cokey, compname, comppct_r,
    hzname, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r,
    wthirdbar_r/100 AS wthirdbar_decimal, wfifteenbar_r/100 AS wfifteenbar_decimal
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    LEFT JOIN component co ON mu.mukey = co.mukey
    LEFT JOIN chorizon ch ON co.cokey = ch.cokey
    WHERE legend.areasymbol LIKE 'TX%'
    ORDER BY musym, co.cokey, ch.hzdept_r ASC;"

s <- SDA_query(q)

nrow(s)
head(s)


# 82 seconds for 84k records
# ~ 1 second per 1k records
system.time(r <- ROSETTA(s, vars = vars))

table(r$.rosetta.model)

str(r)




