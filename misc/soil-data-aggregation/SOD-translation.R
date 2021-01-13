
## Translating the SSURGO ON Demand python / SDA code over the soilDB,
## not sure if we want to go this route, but the code works and is well-tested

# https://github.com/ncss-tech/ssurgoOnDemand/blob/master/SOD/SDA_Properties.py

library(soilDB)

## TODO:
# check use of weights in the presence of missing data
# compare with other aggregation methods
# compare with WSS
# adapt to use mukey | areasymbol
# multiple properties at the same via self-joins
# 




areaReq <- format_SQL_in_statement('CA630')
aProp <- 'taxsuborder'


# dominant component category
qq <- paste0(
"
SELECT areasymbol, musym, muname, mu.mukey  AS mukey, ", aProp , " AS ", aProp, "
FROM legend  AS l
INNER JOIN  mapunit AS mu ON mu.lkey = l.lkey
AND l.areasymbol IN ", areaReq, "
INNER JOIN component AS c ON c.mukey = mu.mukey
AND c.cokey =
(SELECT TOP 1 c1.cokey FROM component AS c1
INNER JOIN mapunit ON c.mukey=mapunit.mukey AND c1.mukey=mu.mukey ORDER BY c1.comppct_r DESC, c1.cokey)
"
)

x <- SDA_query(qq)

head(x)



areaReq <- format_SQL_in_statement('CA630')
tDep <- 0
bDep <- 25
aProp <- 'claytotal_r'

# weighted mean
qq <- paste0(
"
SELECT areasymbol, musym, muname, mukey
INTO #kitchensink
FROM legend  AS lks
INNER JOIN  mapunit AS muks ON muks.lkey = lks.lkey AND lks.areasymbol IN ", areaReq, "
SELECT mu1.mukey, cokey, comppct_r,
SUM (comppct_r) over(partition by mu1.mukey ) AS SUM_COMP_PCT
INTO #comp_temp
FROM legend  AS l1
INNER JOIN  mapunit AS mu1 ON mu1.lkey = l1.lkey AND l1.areasymbol IN ",  areaReq, "
INNER JOIN  component AS c1 ON c1.mukey = mu1.mukey AND majcompflag = 'Yes'
SELECT cokey, SUM_COMP_PCT, CASE WHEN comppct_r = SUM_COMP_PCT THEN 1
ELSE CAST (CAST (comppct_r AS  decimal (5,2)) / CAST (SUM_COMP_PCT AS decimal (5,2)) AS decimal (5,2)) END AS WEIGHTED_COMP_PCT
INTO #comp_temp3
FROM #comp_temp
SELECT
areasymbol, musym, muname, mu.mukey/1  AS MUKEY, c.cokey AS COKEY, ch.chkey/1 AS CHKEY, compname, hzname, hzdept_r, hzdepb_r, CASE WHEN hzdept_r <",  tDep, "  THEN ",  tDep, " ELSE hzdept_r END AS hzdept_r_ADJ,
CASE WHEN hzdepb_r > ",  bDep, "  THEN ",  bDep, " ELSE hzdepb_r END AS hzdepb_r_ADJ,
CAST (CASE WHEN hzdepb_r > ", bDep, "  THEN ", bDep, " ELSE hzdepb_r END - CASE WHEN hzdept_r <",  tDep, " THEN ",  tDep, " ELSE hzdept_r END AS decimal (5,2)) AS thickness,
comppct_r,
CAST (SUM (CASE WHEN hzdepb_r > ",  bDep, "  THEN ",  bDep, " ELSE hzdepb_r END - CASE WHEN hzdept_r <",  tDep, " THEN ",  tDep, " ELSE hzdept_r END) over(partition by c.cokey) AS decimal (5,2)) AS sum_thickness,
CAST (ISNULL (",  aProp, ", 0) AS decimal (5,2)) AS ",  aProp, "
INTO #main
FROM legend  AS l
INNER JOIN  mapunit AS mu ON mu.lkey = l.lkey AND l.areasymbol IN ",  areaReq, "
INNER JOIN  component AS c ON c.mukey = mu.mukey
INNER JOIN chorizon AS ch ON ch.cokey=c.cokey AND hzname NOT LIKE '%O%'AND hzname NOT LIKE '%r%'
AND hzdepb_r >",  tDep, " AND hzdept_r <",  bDep, "
INNER JOIN chtexturegrp AS cht ON ch.chkey=cht.chkey  WHERE cht.rvindicator = 'yes' AND  ch.hzdept_r IS NOT NULL
AND texture NOT LIKE '%PM%' and texture NOT LIKE '%DOM' and texture NOT LIKE '%MPT%' and texture NOT LIKE '%MUCK' and texture NOT LIKE '%PEAT%' and texture NOT LIKE '%br%' and texture NOT LIKE '%wb%'
ORDER BY areasymbol, musym, muname, mu.mukey, comppct_r DESC, cokey,  hzdept_r, hzdepb_r
SELECT #main.areasymbol, #main.musym, #main.muname, #main.MUKEY,
#main.COKEY, #main.CHKEY, #main.compname, hzname, hzdept_r, hzdepb_r, hzdept_r_ADJ, hzdepb_r_ADJ, thickness, sum_thickness, ",  aProp, ", comppct_r, SUM_COMP_PCT, WEIGHTED_COMP_PCT ,
SUM((thickness/sum_thickness ) * ",  aProp, " )over(partition by #main.COKEY)AS COMP_WEIGHTED_AVERAGE
INTO #comp_temp2
FROM #main
INNER JOIN #comp_temp3 ON #comp_temp3.cokey=#main.cokey
ORDER BY #main.areasymbol, #main.musym, #main.muname, #main.MUKEY, comppct_r DESC,  #main.COKEY,  hzdept_r, hzdepb_r
SELECT #comp_temp2.MUKEY,#comp_temp2.COKEY, WEIGHTED_COMP_PCT * COMP_WEIGHTED_AVERAGE AS COMP_WEIGHTED_AVERAGE1
INTO #last_step
FROM #comp_temp2
GROUP BY  #comp_temp2.MUKEY,#comp_temp2.COKEY, WEIGHTED_COMP_PCT, COMP_WEIGHTED_AVERAGE
SELECT areasymbol, musym, muname,
#kitchensink.mukey, #last_step.COKEY,
CAST (SUM (COMP_WEIGHTED_AVERAGE1) over(partition by #kitchensink.mukey) as decimal(5,2))AS ",  aProp, "
INTO #last_step2
FROM #last_step
RIGHT OUTER JOIN #kitchensink ON #kitchensink.mukey=#last_step.mukey
GROUP BY #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname, #kitchensink.mukey, COMP_WEIGHTED_AVERAGE1, #last_step.COKEY
ORDER BY #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname, #kitchensink.mukey
SELECT #last_step2.areasymbol, #last_step2.musym, #last_step2.muname,
#last_step2.mukey, #last_step2.",  aProp, "
FROM #last_step2
LEFT OUTER JOIN #last_step ON #last_step.mukey=#last_step2.mukey
GROUP BY #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, #last_step2.",  aProp, "
ORDER BY #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, #last_step2.",  aProp
)

x <- SDA_query(qq)

