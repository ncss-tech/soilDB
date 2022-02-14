library(soilDB)

# make data/metadata.rda (used by uncode() when NASIS not available)
metadata <- soilDB:::.get_metadata()
save(metadata, file = "data/metadata.rda")
