
## further investigation of wt. mean
## https://github.com/ncss-tech/soilDB/issues/193

library(soilDB)

# an example map unit + components
SDA_query("SELECT mukey, component.cokey, compname, compkind, comppct_r, hzdept_r, hzdepb_r, sandtotal_r FROM component INNER JOIN chorizon ON component.cokey = chorizon.cokey WHERE mukey = '545857' ORDER BY comppct_r DESC, hzdept_r ASC;")

# matches WSS: NA / NULL returned because the largest component has no data
get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                 method = "Dominant Component (Numeric)", 
                 mukeys = 545857,
                 top_depth = 0,
                 bottom_depth = 25)

# does not match WSS
# (sand should be 67.4%, silt should be 19.6%, clay should be 13%)
get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                 method = "Weighted Average", 
                 mukeys = 545857,
                 top_depth = 0,
                 bottom_depth = 25)

# check source data
get_SDA_property(property = c("sandtotal_r","silttotal_r","claytotal_r"),
                 method = "None", 
                 mukeys = 545857,
                 top_depth = 0,
                 bottom_depth = 25)


# incorrect values are re-created when manually performing the wt. avg
# that includes all of the weights
sum(c(60, 30) * c(NA, 67.4), na.rm = TRUE) / sum(c(60, 30))
sum(c(60, 30) * c(NA, 19.6), na.rm = TRUE) / sum(c(60, 30))
sum(c(60, 30) * c(NA, 13), na.rm = TRUE) / sum(c(60, 30))

