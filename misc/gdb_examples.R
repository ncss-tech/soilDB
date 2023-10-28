library(aqp)
library(soilDB)
library(sf)

source("C:/workspace2/github/ncss-tech/soilDB/R/get_component_from_GDB.R")
source("C:/workspace2/github/ncss-tech/soilDB/R/utils.R")


# new GDB examples
dsn <- "D:/geodata/soils/gNATSGO_CONUS_Oct2021/gNATSGO_CONUS.gdb"
dsn <- "D:/geodata/soils/gSSURGO_CONUS_202210.gdb"


# legend ----
le1 <- get_legend_from_GDB(dsn = dsn)
le2 <- get_legend_from_GDB(dsn = dsn, stats = TRUE)
le3 <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'CA%'")
le4 <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'CA%'", stats = TRUE)

le12 <- soilDB::get_legend_from_GDB(dsn = dsn)
le22 <- soilDB::get_legend_from_GDB(dsn = dsn, stats = TRUE)
le32 <- soilDB::get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'CA%'")
le42 <- soilDB::get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'CA%'", stats = TRUE)


daff::diff_data(le1, le12) |> daff::render_diff()
daff::diff_data(le2, le22) |> daff::render_diff()
daff::diff_data(le3, le32) |> daff::render_diff()
daff::diff_data(le4, le42) |> daff::render_diff()


le12 <- get_legend_from_SDA(WHERE = "areasymbol != 'US'")
le32 <- get_legend_from_SDA(WHERE = "areasymbol LIKE 'CA%'")
le42 <- get_legend_from_SDA(WHERE = "areasymbol LIKE 'CA%'")



# mapunit ----
mu1 <- get_mapunit_from_GDB(dsn = dsn)
mu2 <- get_mapunit_from_GDB(dsn = dsn, stats = TRUE)
mu3 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")
mu4 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'", stats = TRUE)
mu5 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'IN%'")
mu6 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'IN%'", stats = TRUE)
mu7 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "compname = 'Miami'")
mu8 <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%' AND areasymbol = 'IN001'")

mu12 <- soilDB::get_mapunit_from_GDB(dsn = dsn)
mu22 <- soilDB::get_mapunit_from_GDB(dsn = dsn, stats = TRUE)
mu32 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")
mu42 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'", stats = TRUE)
mu52 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'IN%'")
mu62 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE 'IN%'", stats = TRUE)
mu72 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "compname = 'Miami'")
mu82 <- soilDB::get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%' AND areasymbol = 'IN001'")

daff::diff_data(mu1, mu12) |> daff::render_diff()
daff::diff_data(mu2, mu22) |> daff::render_diff()
daff::diff_data(mu3, mu32) |> daff::render_diff()
daff::diff_data(mu4, mu42) |> daff::render_diff()
daff::diff_data(mu5, mu52) |> daff::render_diff()
daff::diff_data(mu6, mu62) |> daff::render_diff()



# component ----
co1 <- get_component_from_GDB(dsn)
co2 <- get_component_from_GDB(dsn, childs = TRUE)
co3 <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami' AND majcompflag = 'Yes'", childs = FALSE)

co12 <- soilDB::get_component_from_GDB(dsn)
co22 <- soilDB::get_component_from_GDB(dsn, childs = TRUE)
co32 <- soilDB::get_component_from_GDB(dsn, WHERE = "compname = 'Miami' AND majcompflag = 'Yes'", childs = FALSE)

vars <- names(co12)[names(co12) %in% names(co2)]
summary(d1 <- daff::diff_data(co1[vars], co12))
summary(d3 <- daff::diff_data(co3[vars], co32))


co2 <- co2[order(co2$mukey, co2$cokey), ]
co22 <- co22[order(co22$mukey, co2$cokey), ]
vars2 <- names(co22)[names(co22) %in% names(co2)]
idx <- sample(1:length(co2$mukey), 1000)
all(co2$mukey == co22$mukey)
all(co2$cokey == co22$cpkey)
test1 <- co2[idx, vars2]
test2 <- co22[idx, vars2]
wtf <- daff::diff_data(test1, test2)

test0 <- merge(co2[c("cokey", "pmkind", "pmorigin")], co22[c("cokey", "pmkind", "pmorigin")], by = "cokey", all.x = TRUE)
test0[test0[3] != test0[5] & !is.na(test0[5]), ] |> View()



# fetchGDB ----
f0  <- fetchGDB(dsn = "D:/geodata/soils/gSSURGO_RI_202110/gSSURGO_RI.gdb")
f00 <- fetchGDB(dsn = "D:/geodata/soils/gSSURGO_RI_202110/gSSURGO_RI.gdb", childs = TRUE)
f1 <- fetchGDB(dsn, WHERE = "areasymbol = 'IN001'")
f2 <- fetchGDB(dsn, WHERE = "areasymbol = 'IN001'", childs = TRUE)
# f3 <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'CA%'", childs = TRUE)
f4 <- fetchGDB(dsn, WHERE = "muname LIKE 'Miami%'")
f5 <- fetchGDB(dsn, WHERE = "muname LIKE 'Miami%' AND areasymbol = 'IN001'")
f6 <- fetchGDB(dsn, WHERE = "Compname LIKE 'Miami%'")


f02  <- soilDB::fetchGDB(dsn = "D:/geodata/soils/gSSURGO_RI_202110/gSSURGO_RI.gdb")
f002 <- soilDB::fetchGDB(dsn = "D:/geodata/soils/gSSURGO_RI_202110/gSSURGO_RI.gdb", childs = TRUE)
f12 <- soilDB::fetchGDB(dsn, WHERE = "areasymbol = 'IN001'")
f22 <- soilDB::fetchGDB(dsn, WHERE = "areasymbol = 'IN001'", childs = TRUE)
# f32 <- soilDB::fetchGDB(dsn, WHERE = "areasymbol LIKE 'CA%'", childs = TRUE)
f42 <- soilDB::fetchGDB(dsn, WHERE = "muname LIKE 'Miami%'")
f52 <- soilDB::fetchGDB(dsn, WHERE = "muname LIKE 'Miami%' AND areasymbol = 'IN001'")
f62 <- soilDB::fetchGDB(dsn, WHERE = "Compname LIKE 'Miami%'")

# save(f0, f00, f1, f2, f4, f6, f02, f002, f12, f22, f42, f62, file = "fetchGDA_test.RData")
load(file = "fetchGDA_test.RData")


test <- list(
  list(f0,  f02),
  list(f00, f002),
  list(f1, f12),
  list(f2, f22),
  # list(f3, f32),
  list(f4, f42),
  list(f6, f62)
)


test <- lapply(test, function(x) {
  
  s1 = x[[1]]@site
  s2 = x[[2]]@site
  
  h1 = x[[1]]@horizons
  h2 = x[[2]]@horizons
  
  s <- daff::diff_data(s1, s2)
  h <- daff::diff_data(h1, h2)
  return(list(s = s, h = h))
})



f_sda <- fetchSDA(WHERE = "areasymbol = 'IN001'")

f_cokey <- fetchGDB(dsn, WHERE = "cokey = '22617647'")
f_mi    <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'MI%'")


f <- fetchGDB(dsn, WHERE = "areasymbol LIKE '%'")
save(f_us, file = "gnatsgo.RData")


WHERE <- paste0("areasymbol IN ('", paste0(le2, collapse = "', '"), "')")
system.time(test <- fetchGDB(dsn, WHERE = WHERE))


# formatLandformString vs .cogmd_prep2 ----
vars <- c("cokey", "landscape", "landform", "mntn", "hill","trce", "flats", "shapeacross", "shapedown", "slopeshape", "hillslopeprof")
test <- cogmd[vars]
names(test)[c(3, 6:8)] <- c("geomfname", paste0("geompos", vars[4:6]))
test <- within(test, {
  geomicrorelief = NA
  geommicelev = NA
  geomfmod = NA
  geomfiidref = NA
})
test <- as.data.table(test)
test2 <- test[, soilDB:::.formatLandformString(.SD, uid = .BY$cokey, name.sep = '|'),
              by = list(cokey = test$cokey)]

ed.lf <- data.table::as.data.table(test$geomorph)
lf <- ed.lf[, soilDB:::.formatLandformString(.SD, uid = .BY$peiid, name.sep = '|'),
            by = list(peiid = ed.lf$peiid)]

system.time(.cogmd_prep2(cogmd))




