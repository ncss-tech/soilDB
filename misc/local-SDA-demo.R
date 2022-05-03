library(soilDB)

exdir <- "~/ssurgo_test"
downloadSSURGO(areasymbols = c("CA628", "CA630"), destdir = exdir)
createSSURGO("~/CA628.gpkg", exdir, overwrite = TRUE)

### working
###
get_SDA_coecoclass(areasymbols = "CA628", dsn = "~/CA628.gpkg")
get_SDA_coecoclass(areasymbols = "CA628", method = "dominant component", dsn = "~/CA628.gpkg")
get_SDA_coecoclass(areasymbols = "CA628", method = "dominant condition", dsn = "~/CA628.gpkg")
get_SDA_cosurfmorph(areasymbols = "CA628", table = "cosurfmorphgc", dsn = "~/CA628.gpkg")
get_SDA_cosurfmorph(areasymbols = "CA628", table = "cosurfmorphhpp", dsn = "~/CA628.gpkg")
get_SDA_cosurfmorph(areasymbols = "CA628", table = "cosurfmorphss", dsn = "~/CA628.gpkg") # CONCAT
# TODO: CA628 has no cosurfmorphmr records, so gpkg with just 628 has no cosurfmorphmr table
get_SDA_cosurfmorph(areasymbols = "CA628", table = "cosurfmorphmr", dsn = "~/CA628.gpkg")
get_SDA_muaggatt(areasymbols = "CA628", dsn = "~/CA628.gpkg")
get_SDA_pmgroupname(areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")
get_SDA_pmgroupname(areasymbols = "CA628", method = "dominant component", dsn = "~/CA628.gpkg")
get_SDA_pmgroupname(areasymbols = "CA628", method = "dominant condition", dsn = "~/CA628.gpkg")

# TODO: _very_ slow! because of group_concat()? for "reasons"
get_SDA_interpretation("ENG - Construction Materials; Roadfill", method = "none", areasymbols = "CA628", dsn = "~/CA628.gpkg") # very slow
get_SDA_interpretation("ENG - Construction Materials; Roadfill", areasymbols = "CA628", method = "dominant component", dsn = "~/CA628.gpkg") # very slow
get_SDA_interpretation("ENG - Construction Materials; Roadfill", areasymbols = "CA628", method = "dominant condition", dsn = "~/CA628.gpkg") # very slow

get_SDA_hydric(areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")
get_SDA_property("claytotal_r", areasymbols = "CA628", method = "none", dsn = "~/CA628.gpkg")

### not yet working
###
# get_SDA_interpretation("ENG - Construction Materials; Roadfill", method = "weighted average", areasymbols = "CA628", dsn = "~/CA628.gpkg") # temp tables
# get_SDA_hydric(areasymbols = "CA628", dsn = "~/CA628.gpkg") # temp tables
# get_SDA_property("claytotal_r", method = "dominant component (numeric)", areasymbols = "CA628", dsn = "~/CA628.gpkg") # temp tables

# scratch
# SDA_query("SELECT STRING_AGG(interphrc, '; ')
#                            FROM mapunit AS mu
#                            INNER JOIN component AS c ON c.mukey = 2924882 AND compkind != 'miscellaneous area' AND c.cokey = 21139800
#                            INNER JOIN cointerp AS coi ON c.cokey = coi.cokey AND mu.mukey = 2924882
#                            AND ruledepth != 0 AND interphrc NOT LIKE 'Not%' AND mrulename LIKE 'ENG - Construction Materials; Roadfill'")

