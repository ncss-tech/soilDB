library(soilDB)

exdir <- "~/ssurgo_test"
downloadSSURGO(areasymbols = "CA628", destdir = exdir)
createSSURGO("~/CA628.gpkg", exdir, overwrite = TRUE)

### working
###
get_SDA_coecoclass(areasymbols = "CA628", dsn = "~/CA628.gpkg")
get_SDA_cosurfmorph(areasymbols = "CA628", dsn = "~/CA628.gpkg")
get_SDA_muaggatt(areasymbols = "CA628", dsn = "~/CA628.gpkg")

# NONE aggregation
get_SDA_pmgroupname(areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")
get_SDA_hydric(areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")
get_SDA_property("claytotal_r", areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")
get_SDA_interpretation("ENG - Construction Materials; Roadfill", method = "none", areasymbols = "CA628", dsn = "~/CA628.gpkg") # very slow

# TOP 1/LIMIT 1
get_SDA_interpretation("ENG - Construction Materials; Roadfill", areasymbols = "CA628", method = "dominant component", dsn = "~/CA628.gpkg") # very slow
get_SDA_interpretation("ENG - Construction Materials; Roadfill", areasymbols = "CA628", method = "dominant condition", dsn = "~/CA628.gpkg") # very slow

# CONCAT
get_SDA_cosurfmorph(areasymbols = "CA628", dsn = "~/CA628.gpkg", table = "cosurfmorphss")

### not yet working
###
# get_SDA_pmgroupname(areasymbols = "CA628", dsn = "~/CA628.gpkg") # TOP 1 / temp tables
# get_SDA_hydric(areasymbols = "CA628", dsn = "~/CA628.gpkg") # TOP 1 / temp tables
# get_SDA_interpretation("ENG - Construction Materials; Roadfill", method = "weighted average", areasymbols = "CA628", dsn = "~/CA628.gpkg") # TOP 1 / temp tables
# get_SDA_property("claytotal_r", areasymbols = "CA628", dsn = "~/CA628.gpkg") # TOP 1 / temp tables

# ...
# SDA_query("SELECT STRING_AGG(interphrc, '; ')
#                            FROM mapunit AS mu
#                            INNER JOIN component AS c ON c.mukey = 2924882 AND compkind != 'miscellaneous area' AND c.cokey = 21139800
#                            INNER JOIN cointerp AS coi ON c.cokey = coi.cokey AND mu.mukey = 2924882
#                            AND ruledepth != 0 AND interphrc NOT LIKE 'Not%' AND mrulename LIKE 'ENG - Construction Materials; Roadfill'")

