## code to prepare `metadata` dataset goes here
# make data/metadata.rda (used by uncode() when NASIS not available)
metadata <- soilDB::get_NASIS_metadata()
usethis::use_data(metadata, overwrite = TRUE, compress = 'xz')
