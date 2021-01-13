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

# missing sand -> NA | no model = -1
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
sandtotal_r, silttotal_r, claytotal_r, 
dbthirdbar_r, 
wthirdbar_r/100 AS wthirdbar_decimal, 
wfifteenbar_r/100 AS wfifteenbar_decimal 
FROM component AS co 
LEFT JOIN chorizon AS ch ON co.cokey = ch.cokey ;"

# get data from SDA
s <- SDA_query(qq)

vars <- c(
  'sandtotal_r', 'silttotal_r', 'claytotal_r', 
  'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal'
)

# automatic model selection: 
# 50k records: ~ 39 seconds
r <- ROSETTA(s, vars = vars)



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


# 60 seconds for 84k records
# ~ 1 second per 1k records
system.time(r <- ROSETTA(s, vars = vars))

table(r$.rosetta.model)

str(r)



##
## compare models version 1 and 3
##

library(latticeExtra)
library(hexbin)
library(viridis)

# lots of data, some with missing values
qq <- "SELECT TOP 25000 
compname, hzname, hzdept_r, hzdepb_r, 
sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r, wthirdbar_r/100 AS wthirdbar_decimal, wfifteenbar_r/100 AS wfifteenbar_decimal 
FROM component AS co 
LEFT JOIN chorizon AS ch ON co.cokey = ch.cokey ;"

s <- SDA_query(qq)

str(s)


vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')

# run data through both versions
r1 <- ROSETTA(s, vars = vars, v = "1")
r2 <- ROSETTA(s, vars = vars, v = "2")
r3 <- ROSETTA(s, vars = vars, v = "3")


g <- make.groups(v1 = r1, v2 = r2, v3 = r3)

v <- c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')
w1 <- r1[, v]
w2 <- r2[, v]
w3 <- r3[, v]

names(w1) <- sprintf("%s.1", names(w1))
names(w2) <- sprintf("%s.2", names(w2))
names(w3) <- sprintf("%s.3", names(w3))

w <- cbind(w1, w2, w3)

hexbinplot(
  theta_r.1 ~ theta_r.3, 
  data = w, 
  asp=1, xbins=60,
  xlab = 'Model v3', ylab = 'Model v1',
  main = 'theta_r',
  colramp=viridis, trans=log, inv=exp, colorkey=FALSE,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
    panel.abline(a = 0, b = 1, lwd = 2)
  }
)

hexbinplot(
  theta_s.1 ~ theta_s.3, 
  data = w, 
  asp=1, xbins=60,
  xlab = 'Model v3', ylab = 'Model v1',
  main = 'theta_r',
  colramp=viridis, trans=log, inv=exp, colorkey=FALSE,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
    panel.abline(a = 0, b = 1, lwd = 2)
  }
)

hexbinplot(
  alpha.1 ~ alpha.3, 
  data = w, 
  asp=1, xbins=60,
  xlab = 'Model v3', ylab = 'Model v1',
  main = 'theta_r',
  colramp=viridis, trans=log, inv=exp, colorkey=FALSE,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
    panel.abline(a = 0, b = 1, lwd = 2)
  }
)

hexbinplot(
  npar.1 ~ npar.3, 
  data = w, 
  asp=1, xbins=60,
  xlab = 'Model v3', ylab = 'Model v1',
  main = 'theta_r',
  colramp=viridis, trans=log, inv=exp, colorkey=FALSE,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
    panel.abline(a = 0, b = 1, lwd = 2)
  }
)


hexbinplot(
  ksat.1 ~ ksat.3, 
  data = w, 
  asp=1, xbins=60,
  xlab = 'Model v3', ylab = 'Model v1',
  main = 'theta_r',
  colramp=viridis, trans=log, inv=exp, colorkey=FALSE,
  panel = function(...) {
    panel.grid(-1, -1)
    panel.hexbinplot(...)
    panel.abline(a = 0, b = 1, lwd = 2)
    
  }
)


hexplom(
  w[, c('theta_r.1', 'theta_r.2', 'theta_r.3')], 
  trans = log, 
  colramp = viridis, 
  upper.panel = panel.hexboxplot,
  main = 'ROSETTA Evaluation: theta_r',
  xlab = ''
)

hexplom(
  w[, c('theta_s.1', 'theta_s.2', 'theta_s.3')], 
  trans = log, 
  colramp = viridis, 
  upper.panel = panel.hexboxplot,
  main = 'ROSETTA Evaluation: theta_s',
  xlab = ''
)

hexplom(
  w[, c('alpha.1', 'alpha.2', 'alpha.3')], 
  trans = log, 
  colramp = viridis, 
  upper.panel = panel.hexboxplot,
  main = 'ROSETTA Evaluation: alpha',
  xlab = ''
)

hexplom(
  w[, c('npar.1', 'npar.2', 'npar.3')], 
  trans = log, 
  colramp = viridis, 
  upper.panel = panel.hexboxplot,
  main = 'ROSETTA Evaluation: npar',
  xlab = ''
)
