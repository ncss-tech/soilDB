# make RV copedon snapshot work with fetchNASIS / SQLite backend

library(soilDB)

SS <- FALSE
nullFragsAreZero <- TRUE
stringsAsFactors <- FALSE

dsn <- "C:/Geodata/soils/NASIS-data_patched.sqlite"
copedon_rv_data <- "C:/Geodata/soils/copedon-data.rds"
copedon <- readRDS(copedon_rv_data)

### 
### BEGIN -- PATCHES TO THE NASIS-data.sqlite "Pedon Snapshot" for fetchNASIS
### 
# res1 <- dbQueryNASIS(soilDB:::.openNASISchannel(dsn),
#                      "SELECT geomfname, geomfiid, geomftiidref FROM geomorfeat")
# res2 <- dbQueryNASIS(soilDB:::.openNASISchannel(dsn),
#                      "SELECT geomftname, geomftiid FROM geomorfeattype")
# 
# # # get the geomorphic features tables in the SQLite snapshot
# library(DBI)
# con <- DBI::dbConnect(RSQLite::SQLite(), dsn)
# DBI::dbWriteTable(con, "geomorfeat", res1, overwrite = TRUE)
# DBI::dbWriteTable(con, "geomorfeattype", res2, overwrite = TRUE)
# DBI::dbDisconnect(con)

# # fix the standard lat/long/gps names truncated to 16 characters 
# con <- DBI::dbConnect(RSQLite::SQLite(), dsn)
# sitetab <- DBI::dbReadTable(con, "site")
# sitetab$longstddecimaldegrees <- sitetab$longstddecimalde
# sitetab$latstddecimaldegrees <- sitetab$latstddecimaldeg
# sitetab$gpspositionalerror <- sitetab$gpspositionalerr
# DBI::dbWriteTable(con, "site", sitetab, overwrite = TRUE)
# DBI::dbDisconnect(con)

# # fix the petaxhistory record ID name truncated to 16 characters 
# con <- DBI::dbConnect(RSQLite::SQLite(), dsn)
# petaxmtab <- DBI::dbReadTable(con, "petaxhistmoistcl")
# petaxmtab$pedtaxhistoryiidref <- petaxmtab$pedtaxhistoryiid
# DBI::dbWriteTable(con, "petaxhistmoistcl", petaxmtab, overwrite = TRUE)
# petaxotab <- DBI::dbReadTable(con, "petxhistfmother")
# petaxotab$pedtaxhistoryiidref <- petaxotab$pedtaxhistoryiid
# DBI::dbWriteTable(con, "petxhistfmother", petaxotab, overwrite = TRUE)
# DBI::dbDisconnect(con)

# # fix the different names in MetadataDomainDetail
# con <- DBI::dbConnect(RSQLite::SQLite(), dsn)
# metatab <- DBI::dbReadTable(con, "MetadataDomainDetail")
# metatab$DomainID <- metatab$domain_id
# metatab$ChoiceValue <- metatab$choice_id
# metatab$ChoiceName <- metatab$choice_label
# DBI::dbWriteTable(con, "MetadataDomainDetail", metatab, overwrite = TRUE)
# DBI::dbDisconnect(con)

###
### END PATCHES TO PEDON SNAPSHOT for fetchNASIS
###

system.time(f <- fetchNASIS(
                SS = SS,
                dsn = dsn,
                rmHzErrors = FALSE
              ))
# TODO: handle uncoding options

library(aqp)
aqp_df_class(f) <- "data.table"
f <- rebuildSPC(f)
save(f, file = "C:/Geodata/soils/fetchNASIS-data-2.rda")
# load("C:/Geodata/soils/fetchNASIS-data-2.rda")

system.time(good.ids <- checkHzDepthLogic(f, fast = TRUE))
save(good.ids, file = "C:/Geodata/soils/fetchNASIS-data-goodids-2.rda")

f.sub <- subset(f, good.ids$valid)
all(good.ids$valid)
site(f.sub) <- good.ids
all(f.sub$valid)

## from full set, you can do subset operations on any site level var
mollisols <- subset(f, f$taxorder == "mollisols")

## here, we match peiid against a lookup table of RV component pedon peiids
f.cp <- subset(f.sub, profile_id(f.sub) %in% unique(copedon$peiid))

library(dplyr)

site(f.cp) %>%
  count(taxgrtgroup) %>%
  filter(n > 30)

# calculate the expected MTR for ~27k RV copedons
res <- profileApply(f.cp, mollic.thickness.requirement, clay.attr = 'clay')
f.cp$mollic_thickness_requirement <- res

## 27790 elements
save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon-2.rda")

copedon_rv_data <- "C:/Geodata/soils/copedon-data.rds"
copedon <- readRDS(copedon_rv_data)
copedon$peiid <- as.character(copedon$peiid)
copedon$phiid <- as.character(copedon$phiid)

# add mixed moist color information
horizons(f.cp)$phiid <- as.character(horizons(f.cp)$phiid)
horizons(f.cp) <- copedon[,c("phiid","peiid",
                             "mxhue_moist","mxvalue_moist","mxchroma_moist",
                             "mxhue_dry","mxvalue_dry","mxchroma_dry")]

f.cp$m_hue <- f.cp$mxhue_moist
f.cp$m_value <- f.cp$mxvalue_moist
f.cp$m_chroma <- f.cp$mxchroma_moist
f.cp$d_hue <- f.cp$mxhue_dry
f.cp$d_value <- f.cp$mxvalue_dry
f.cp$d_chroma <- f.cp$mxchroma_dry
f.cp$soil_color <- with(horizons(f.cp), munsell2rgb(m_hue, m_value, m_chroma))
save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon-2.rda")

f.cp$is_mollic_color <- hasDarkColors(f.cp, d_value = NA)

darkColorInterval <- function(p) {
  mss <- getMineralSoilSurfaceDepth(p)
  p.sub <- glom(p, mss, estimateSoilDepth(p))
  if (!inherits(p.sub, 'SoilProfileCollection')) {
    return(data.frame(peiid = profile_id(p),
                        mineral_surface = mss,
                        darkness_depth = NA))
  }
  return(data.frame(peiid = profile_id(p),
                    mineral_surface = mss,
                    darkness_depth = getSurfaceHorizonDepth(p.sub, pattern = "TRUE", 
                                                            hzdesgn = "is_mollic_color"
          )))
}
dcdf <- profileApply(f.cp, darkColorInterval, frameify = TRUE)
f.cp$mineral_surface <- NULL
f.cp$darkness_depth <- NULL
site(f.cp) <- dcdf
save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon-2.rda")
