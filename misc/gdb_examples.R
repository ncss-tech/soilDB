library(soilDB)
library(sf)

# new GDB examples
dsn <- "D:/geodata/soils/gNATSGO_CONUS.gdb"

le <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE '%'")

mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")

co <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami' AND majcompflag = 'Yes'", childs = FALSE)

f_in <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'IN%'")
f_ca <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'CA%'")
f_mi <- fetchGDB(dsn, WHERE = "areasymbol LIKE 'MI%'")

le <- le[order(le$areasymbol), ]
f_us <- lapply(le$areasymbol[2313:nrow(le)], function(x) {
    WHERE <- paste0("areasymbol = '", x, "'")
  f <- fetchGDB(dsn, WHERE = WHERE)
})

test <- fetchGDB(dsn, WHERE = "areasymbol = 'OR649'")
