library(aqp)
library(soilDB)
library(httr)
library(jsonlite)


# brief manual page
?ROSETTA

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
ROSETTA(x, vars = c('sand', 'silt', 'clay'), conf = httr::verbose())



## stress test

# lots of data, some with missing values
qq <- "SELECT TOP 50000 compname, hzname, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r FROM component AS co JOIN chorizon AS ch ON co.cokey = ch.cokey WHERE claytotal_r IS NOT NULL ;"

s <- SDA_query(qq)

str(s)

# ~ 50k records: ~55 seconds
system.time(r <- ROSETTA(s, vars = c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r'))

str(r)
summary(r)



# lots of data, some with missing values
q <- "SELECT musym, co.cokey, compname, comppct_r,
    hzname, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, dbthirdbar_r,
    wthirdbar_r/100 AS wthirdbar_decimal, wfifteenbar_r/100 AS wfifteenbar_decimal
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    INNER JOIN component co ON mu.mukey = co.mukey
    INNER JOIN chorizon ch ON co.cokey = ch.cokey
    WHERE legend.areasymbol LIKE 'TX%'
    ORDER BY musym, co.cokey, ch.hzdept_r ASC;"

s <- SDA_query(q)

nrow(s)
head(s)


vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')

s[42, vars]
ROSETTA(s[41:42, ], vars = vars)



# 70 seconds for 63k records
# ~ 1 second per 1k records
system.time(r <- ROSETTA(s, vars = vars))

str(r)
summary(r)


## Todd will implement server-side

# 
# # 
# # model.2 <- which(complete.cases(s[,  c('sandtotal_r', 'silttotal_r', 'claytotal_r')]))
# # model.3 <- which(complete.cases(s[,  c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r')]))
# # model.4 <- which(complete.cases(s[,  c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal')]))
# # model.5 <- which(complete.cases(s[,  c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')]))
# 
# 
# 
# ## split into model chunks
# na.res <- sapply(s[, vars], is.na)
# head(na.res)
# nrow(na.res)
# 
# # required fields: sand, silt, clay
# ssc.eval <- ! apply(
#   na.res[, c('sandtotal_r', 'silttotal_r', 'claytotal_r')], 
#   1,
#   all
#   )
# 
# model.2 <- which(
#   ssc.eval & 
#     (na.res[, 'dbthirdbar_r'] | na.res[, 'wthirdbar_decimal'] | na.res[, 'wfifteenbar_decimal'] )
#   )
# 
# 
# model.3 <- which(
#   ssc.eval & 
#     (na.res[, 'wthirdbar_decimal'] | na.res[, 'wfifteenbar_decimal'] )
# )
# 
# 
# model.5 <- which(apply(na.res, 1, all))
# 
# length(model.2)
# length(model.3)
# length(model.4)
# length(model.5)
# 
# sum(
#   c(
#     length(model.2),
#     length(model.3),
#     length(model.4),
#     length(model.5)
#   )
# )
# 
# nrow(s)
# 
# # s[model.2,]
# 
# 
# 



