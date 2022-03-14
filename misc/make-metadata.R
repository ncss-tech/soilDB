library(soilDB)

# make data/metadata.rda (used by uncode() when NASIS not available)
metadata <- soilDB:::.get_NASIS_metadata()
save(metadata, file = "data/metadata.rda")

head(metadata)
