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


f_us <- lapply(sample(le$areasymbol, 30), function(x) {
    WHERE <- paste0("areasymbol = '", x, "'")
  f <- fetchGDB(dsn, WHERE = WHERE)
})
f_us <- aqp::union(f_us)
length(f_us)


test <- fetchGDB(dsn, WHERE = "areasymbol IN ('OR649', 'US')")
