library(sf)
library(soilDB)

rssdir <- "D:/Geodata/RSS"
.rasterPath <- function(statecode) sprintf("D:/Geodata/rss-mukey_state_10m/rss-mukey_%s_10m_trim.tif", statecode)
srd <- list.dirs(rssdir, recursive = TRUE)
gdbs <- srd[grepl("\\.gdb", srd)]

for (g in gdbs) {
  st8 <- gsub(".*/rss_([a-z]{2})\\.gdb", "\\1", g)
  res <- soilDB:::.dumpSSURGOGDB(g, dirname(g), replace_names = c("SAPOLYGON" = sprintf("soilsa_a_%s000", st8)))
  gpkg <- file.path(rssdir, sprintf("rss_%s.gpkg", st8))
  suppressWarnings(file.remove(gpkg))
  # write raster first; create gpkg new
  sf::gdal_utils("translate", .rasterPath(st8), gpkg, options = c("-ot", "Float32",
                                                                  "-co", "APPEND_SUBDATASET=YES",
                                                                  # "-co", "RASTER_TABLE=mukey",
                                                                  "-co", "FIELD_NAME=mukey"))
  createSSURGO(gpkg, exdir = sprintf("D:/Geodata/RSS/rss_%s/rss_%s/", st8, st8), overwrite = FALSE, header = TRUE)
}

