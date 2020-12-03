# devtools::install()

library(soilDB)

# SS <- FALSE
# nullFragsAreZero <- TRUE
# stringsAsFactors <- FALSE
# 
# nasislite_path <- "C:/Geodata/soils/NASIS-data.sqlite"
# copedon_rv_data <- "C:/Geodata/soils/copedon-data.rds"
# copedon <- readRDS(copedon_rv_data)

# res1 <- dbQueryNASIS(soilDB:::.openNASISchannel(nasislite_path),
#                      "SELECT geomfname, geomfiid, geomftiidref FROM geomorfeat")
# res2 <- dbQueryNASIS(soilDB:::.openNASISchannel(nasislite_path),
#                      "SELECT geomftname, geomftiid FROM geomorfeattype")

# # quick hack to get the geomorphic features tables in there
# library(DBI)
# con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Geodata/soils/NASIS-data.sqlite")
# DBI::dbWriteTable(con, "geomorfeat", res1, overwrite = TRUE)
# DBI::dbWriteTable(con, "geomorfeattype", res2, overwrite = TRUE)
# DBI::dbDisconnect(con)

# extended_data <- get_extended_data_from_NASIS_db(SS = SS,
#                                                  sqlite_path = nasislite_path,
#                                                  nullFragsAreZero = nullFragsAreZero,
#                                                  stringsAsFactors = stringsAsFactors)
# f <- fetchNASIS(SS = SS, sqlite_path = nasislite_path)
# 
# library(aqp)
# aqp_df_class(f) <- "data.table"
# f <- rebuildSPC(f)
# save(f, file = "C:/Geodata/soils/fetchNASIS-data.rda")
# load("C:/Geodata/soils/fetchNASIS-data.rda")

# good.ids <- checkHzDepthLogic(f)
# save(good.ids, file = "C:/Geodata/soils/fetchNASIS-data-goodids.rda")

# f.sub <- subset(f, good.ids$valid)
# all(good.ids$valid)
 
## from full set, you can do subset operations on any site level var
# mollisols <- subset(f, f$taxorder == "Mollisols")

## here, we match peiid against a lookup table of RV component pedon peiids
# f.cp <- subset(f, profile_id(f) %in% unique(copedon$peiid))

# library(dplyr)
# 
# site(f.cp) %>%
#   count(taxgrtgroup) %>%
#   filter(n > 30)

# calculate the expected MTR for ~27k RV copedons
# res <- profileApply(f.cp, mollic.thickness.requirement, clay.attr = 'clay')
# f.cp$mollic_thickness_requirement <- res

## 27790 elements
# save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon.rda")
# 
# 
# color analysis
# library(aqp)
load("C:/Geodata/soils/fetchNASIS-rv-copedon.rda")
# 
# copedon_rv_data <- "C:/Geodata/soils/copedon-data.rds"
# copedon <- readRDS(copedon_rv_data)
# copedon$peiid <- as.character(copedon$peiid)
# copedon$phiid <- as.character(copedon$phiid)
# 
# # add mixed moist color information
# horizons(f.cp)$phiid <- as.character(horizons(f.cp)$phiid)
# horizons(f.cp) <- copedon[,c("phiid","peiid",
#                              "mxhue_moist","mxvalue_moist","mxchroma_moist",
#                              "mxhue_dry","mxvalue_dry","mxchroma_dry")]

# f.cp$m_hue <- f.cp$mxhue_moist
# f.cp$m_value <- f.cp$mxvalue_moist
# f.cp$m_chroma <- f.cp$mxchroma_moist
# f.cp$d_hue <- f.cp$mxhue_dry
# f.cp$d_value <- f.cp$mxvalue_dry
# f.cp$d_chroma <- f.cp$mxchroma_dry
# f.cp$soil_color <- with(horizons(f.cp), munsell2rgb(m_hue, m_value, m_chroma))
# save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon.rda")

# f.cp$is_mollic_color <- hasDarkColors(f.cp, d_value = NA)
# 
# darkColorInterval <- function(p) {
#   mss <- getMineralSoilSurfaceDepth(p)
#   p.sub <- glom(p, mss, estimateSoilDepth(p))
#   if (!inherits(p.sub, 'SoilProfileCollection')) {
#     return(data.frame(peiid = profile_id(p),
#                         mineral_surface = mss,
#                         darkness_depth = NA))
#   }
#   return(data.frame(peiid = profile_id(p),
#                     mineral_surface = mss,
#                     darkness_depth = getSurfaceHorizonDepth(p.sub, pattern = "TRUE", hzdesgn = "is_mollic_color"
#           )))
# }
# dcdf <- profileApply(f.cp, darkColorInterval, frameify = TRUE)
# f.cp$mineral_surface <- NULL
# f.cp$darkness_depth <- NULL
# site(f.cp) <- dcdf
# save(f.cp, file = "C:/Geodata/soils/fetchNASIS-rv-copedon.rda")

load("C:/Geodata/soils/fetchNASIS-rv-copedon.rda")

library(aqp)
library(sf)
library(ggplot2)
library(rnaturalearth)

dat <- site(f.cp)

vplotdat <- subset(dat, dat$mollic_thickness_requirement >= 18)
vioplot::vioplot(vplotdat$mollic_thickness_requirement ~ vplotdat$taxmoistcl)

do.call('rbind', lapply(split(dat, dat$taxmoistcl), function(x) {
    quantile(x$mollic_thickness_requirement, na.rm = TRUE, probs = c(0.4,0.5,0.6,0.7,0.8,0.9))
  }))

library(dplyr, warn.conflicts = FALSE)
dat <- dat %>% mutate(mtr_group = case_when(mollic_thickness_requirement < 18 ~ "10 cm",
                                            mollic_thickness_requirement == 18 ~ "18 cm",
                                            mollic_thickness_requirement > 18 &
                                              mollic_thickness_requirement < 25 ~ "18 to 25 cm",
                                            mollic_thickness_requirement == 25 ~ "25 cm"))
dsplit <- split(dat, dat$mtr_group)

datfilt <- dat[!is.na(dat$mtr_group), ]
datfilt$met_mollic_moist <- (datfilt$darkness_depth >= datfilt$mollic_thickness_requirement)
datfilt$met_20cm_moist <- (datfilt$darkness_depth >= 20)
datfilt$met_25cm_moist <- (datfilt$darkness_depth >= 25)
datfilt$met_30cm_moist <- (datfilt$darkness_depth >= 30)

dat.sp <- datfilt[,c("peiid", "x_std", "y_std", "mtr_group",
                     "met_mollic_moist",
                     "met_20cm_moist", "met_25cm_moist", "met_30cm_moist")]

dat.sp <- st_as_sf(dat.sp[complete.cases(dat.sp),], coords = c("x_std", "y_std"), crs = 4326)

plot(subset(dat.sp[dat.sp$peiid %in% datfilt$peiid,'met_mollic_moist'], met_mollic_moist == TRUE))

world <- ne_countries(scale = "medium", returnclass = "sf")
world$.id <- 1
world <- merge(world, data.frame(id = 1, mtr_group = unique(datfilt$mtr_group)))

usa <- subset(world, admin == "United States of America")
dat.sp <- st_crop(dat.sp, usa)

ggplot(data = datfilt, aes(group = mtr_group)) +
  facet_wrap(~ mtr_group, nrow = 2) +
  scale_color_viridis_d(direction = -1) + 
  ggtitle(sprintf('Mollic Epipedon Thickness Requirements in NASIS Component Representative Pedons (n = %s)', 
                  nrow(dat.sp))) +
  geom_sf(data = usa, fill = "#93dfb8") +
  geom_sf(data =  dat.sp[dat.sp$peiid %in% dsplit[[1]]$peiid,], 
          size = 0.005, aes(color = met_mollic_moist)) +
  geom_sf(data =  dat.sp[dat.sp$peiid %in% dsplit[[2]]$peiid,], 
          size = 0.005, aes(color = met_mollic_moist)) +
  geom_sf(data =  dat.sp[dat.sp$peiid %in% dsplit[[3]]$peiid,], 
          size = 0.005, aes(color = met_mollic_moist)) +
  geom_sf(data =  dat.sp[dat.sp$peiid %in% dsplit[[4]]$peiid,], 
          size = 0.005, aes(color = met_mollic_moist)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  labs(color = "Meets Moist Color\n") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))


datfilt_wasmollic <- filter(datfilt, met_mollic_moist == TRUE)
dat.sp_wasmollic <- filter(dat.sp, dat.sp$peiid %in% datfilt_wasmollic$peiid)
ggplot(data = datfilt_wasmollic, aes(group = mtr_group)) +
  facet_wrap(~ mtr_group, nrow = 2) +
  scale_color_viridis_d(direction = -1) + 
  ggtitle(sprintf('Mollic Epipedon Thickness Requirements in NASIS Component Representative Pedons (n = %s)', 
                  nrow(dat.sp_wasmollic))) +
  geom_sf(data = usa, fill = "#93dfb8") +
  geom_sf(data =  dat.sp_wasmollic[dat.sp_wasmollic$peiid %in% dsplit[[1]]$peiid,],
          size = 0.005, aes(color = met_25cm_moist)) +
  geom_sf(data =  dat.sp_wasmollic[dat.sp_wasmollic$peiid %in% dsplit[[2]]$peiid,],
          size = 0.005, aes(color = met_25cm_moist)) +
  geom_sf(data =  dat.sp_wasmollic[dat.sp_wasmollic$peiid %in% dsplit[[3]]$peiid,],
          size = 0.005, aes(color = met_25cm_moist)) +
  geom_sf(data =  dat.sp_wasmollic[dat.sp_wasmollic$peiid %in% dsplit[[4]]$peiid,],
          size = 0.005, aes(color = met_25cm_moist)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) + 
  labs(color = "Meets Moist Color (25cm)\n") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))

res <- subset(datfilt_wasmollic, !met_25cm_moist)
sort(table(res$taxgrtgroup))
to.update <- subset(f.cp, peiid %in% res$peiid)

taxonnames <- sort(table(to.update$taxonname))


plot(subset(to.update, taxonname %in% names(taxonnames[taxonnames >= 2])))

plot_series <- function(series_name) {
  print(series_name)
  if (series_name != "SND") {
    ful <- subset(f.cp, taxonname == series_name)
    upd <- subset(to.update, taxonname == series_name)
    osd <- try(fetchOSD(series_name))
    if (!is.null(osd)) {
      ser <- aqp::combine(osd, subset(ful, !(peiid %in% upd$peiid)), upd)
      ser$threshold[ser$darkness_depth >= 25] <- "Meets Requirement"
      ser$threshold[ser$darkness_depth < 25] <- "Does Not Meet Requirement"
      ser$threshold[is.na(ser$threshold)] <- "OSD"
      groupedProfilePlot(ser, groups = "threshold", cex.names = 0.6, max.depth = 200, print.id=FALSE)
      abline(h = 25, lty=3, lwd=1, col="red")
      title(sprintf("%s (%s)", series_name, osd$family))
    } else {
      stop(osd)
    }
  }
}

lapply(tail(names(taxonnames[taxonnames >= 2]), 20), plot_series)

update_mukeys <- unique(copedon[copedon$peiid %in% to.update$peiid, 'nationalmusym'])

mupoly <- read_sf("E:/Geodata/soils/MLRA_2_SON_FY2021.gdb", "mupolygon")

mupoly_sub <- subset(mupoly, mupoly$NATMUSYM %in% update_mukeys)
area <- st_area(st_union(mupoly_sub$Shape))
units(area) <- "acres"
area

plot(mupoly_sub$Shape)

mupoly_sub <- merge(mupoly_sub, copedon, all.x=TRUE)
res <- subset(f.cp, peiid %in% mupoly_sub$peiid)
site(res) <- mupoly_sub[,c("compname","comppct_r","peiid")]

res$compname
res_moll <- subset(res, taxorder == "Mollisols")

res_moll$mollic_thickness_requirement - res_moll$darkness_depth
(25 - res_moll$darkness_depth) > 0

res_moll20 <- subset(res_moll, (20 - res_moll$darkness_depth) > 0)

plot(res_moll20, label = "compname")
abline(h=18)


res_inc <- subset(res, taxorder == "Inceptisols")

res_inc$mollic_thickness_requirement - res_inc$darkness_depth
(25 - res_inc$darkness_depth) > 0

res_inc20 <- subset(res_inc, (20 - res_inc$darkness_depth) > 0)

plot(res_inc20, label = "compname")
abline(h=18)
res_inc20$taxgrtgroup
