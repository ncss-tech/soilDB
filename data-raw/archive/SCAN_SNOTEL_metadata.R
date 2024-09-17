## code to prepare `SCAN_SNOTEL_metadata` dataset goes here
SCAN_SNOTEL_metadata <- read.csv("data-raw/station-metadata.csv", row.names = FALSE)

usethis::use_data(SCAN_SNOTEL_metadata, overwrite = TRUE, compress = 'xz')
