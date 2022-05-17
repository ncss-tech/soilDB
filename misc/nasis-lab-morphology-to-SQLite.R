# create Lab Morphology SQLite from Geodatabase
# 

library(soilDB)

exdir <- "D:/Geodata/soils/NASIS_Morphological"
soilDB:::.dumpSSURGOGDB(dsn = "D:/Geodata/soils/NASIS_Morphological_20200116.gdb/NASIS_Morphological_20200116.gdb",
                        exdir = exdir)

# throw out the metadata included with the GDB, add lookup tables and area table
for (xn in get_NASIS_table_name_by_purpose(c("metadata","lookup","area"))) {
  d <- dbQueryNASIS(NASIS(), paste("SELECT * FROM", xn))
  write.table(
    d,
    file = file.path(exdir, "tabular", paste0(xn, ".txt")),
    sep = "|",
    qmethod = "double",
    col.names = TRUE,
    row.names = FALSE
  )
}

# Pipe-delimited TXT (with headers) -> SQLite
createSSURGO("D:/Geodata/soils/NASIS_Morphological/nasis_morph.sqlite", 
             "D:/Geodata/soils/NASIS_Morphological/", header = TRUE)

# convert names and labels to numeric codes -> standard NASIS static db (storing coded values)
soilDB:::.recode_db("D:/Geodata/soils/NASIS_Morphological/nasis_morph.sqlite")
file.copy("D:/Geodata/soils/NASIS_Morphological/nasis_morph.sqlite",
          "D:/Geodata/soils/NASIS_Morphological/nasis_morph_decoded.sqlite")

# test that it works
f <- fetchNASIS(dsn = "D:/Geodata/soils/NASIS_Morphological/nasis_morph.sqlite", SS = FALSE, mixColors = FALSE)

# convert numeric codes to names -> a "decoded" NASIS static db (storing user readable values)
soilDB:::.decode_db("D:/Geodata/soils/NASIS_Morphological/nasis_morph_decoded.sqlite")

# test that it works
options(soilDB.NASIS.skip_uncode = TRUE)
f <- fetchNASIS(dsn = "D:/Geodata/soils/NASIS_Morphological/nasis_morph_decoded.sqlite", SS = FALSE, mixColors = FALSE)
