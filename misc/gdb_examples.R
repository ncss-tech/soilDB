library(soilDB)
library(sf)

# new GDB examples
dsn <- "D:/geodata/soils/gNATSGO_CONUS.gdb"

le <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE '%'")
le <- le[order(le$areasymbol), ]

mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")

co <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami' AND majcompflag = 'Yes'", childs = FALSE)

f_in <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'IN%'")
f_ca <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'CA%'")
f_mi <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'MI%'")


f <- fetchGDB(dsn, WHERE = "areasymbol LIKE '%'")
save(f_us, file = "gnatsgo.RData")


WHERE <- paste0("areasymbol IN ('", paste0(le2, collapse = "', '"), "')")
system.time(test <- fetchGDB(dsn, WHERE = WHERE))
