library(soilDB)
library(sf)

# new GDB examples
dsn <- "D:/geodata/soils/gNATSGO_CONUS.gdb"

le <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE '%'")

mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami%'")

co <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami' AND majcompflag = 'Yes'", childs = FALSE)

system.time(suppressMessages(
  f_in_SDA <- fetchSDA(WHERE = "areasymbol LIKE 'IN%'", duplicates = TRUE)
))
system.time(suppressMessages(
  f_in_GDB <- fetchGDB(WHERE = "areasymbol LIKE 'IN%'")
))
