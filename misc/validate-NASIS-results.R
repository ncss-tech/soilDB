## validation of NASIS-functionality for nasisDBI PR
## git shell command to list files changed in the PR

# git diff --name-only master...nasisDBI | grep -E get\\|NASIS\\|fetch

f <- read.table(text = "R/fetchNASIS.R
                        R/fetchNASIS_pedons.R
                        R/getHzErrorsNASIS.R
                        R/get_RMF_from_NASIS_db.R
                        R/get_colors_from_NASIS_db.R
                        R/get_component_data_from_NASIS_db.R
                        R/get_concentrations_from_NASIS_db.R
                        R/get_cosoilmoist_from_NASIS.R
                        R/get_extended_data_from_NASIS_db.R
                        R/get_hz_data_from_NASIS_db.R
                        R/get_lablayer_data_from_NASIS_db.R
                        R/get_labpedon_data_from_NASIS_db.R
                        R/get_phfmp_from_NASIS_db.R
                        R/get_phlabresults_data_from_NASIS_db.R
                        R/get_projectmapunit_from_NASIS.R
                        R/get_site_data_from_NASIS_db.R
                        R/get_text_notes_from_NASIS_db.R
                        R/get_veg_data_from_NASIS_db.R
                        R/get_vegplot_data_from_NASIS_db.R 
                        R/openNASISchannel.R")$V1
                        # R/dbQueryNASIS.R
                        # R/get_soilseries_from_NASIS.R
                        # man/dbConnectNASIS.Rd
                        # man/dbQueryNASIS.Rd
                        # man/fetchNASIS.Rd
                        # man/getHzErrorsNASIS.Rd
                        # misc/man-deprecated/fetchNASIS.Rd
                        # 
# # you want the version of soilDB you are testing to be installed
# devtools::install()

library(soilDB)

fnames <- sapply(f, function(x) { 
  spv <- evalSource(x, package = "soilDB")
  # source(x)
  names(as.list(spv)) 
})

test <- lapply(fnames, function(fname) {
    lapply(fname, function(FUN) {
      message(sprintf("Testing: %s", FUN))
      try(get(FUN, envir = as.environment("package:soilDB"))())
    })
  })

res <- unlist(lapply(names(test), function(x) lapply(seq_along(test[[x]]), function(y) {
      res <- inherits(test[[x]][[y]], 'try-error')
      names(res) <- fnames[[x]][y]
      res
    })))

res[which(res)]

# Fixed: Text fields must come at end of query per MSSQL specs
# get_text_notes_from_NASIS_db()

# Fixed: same as above
# get_vegplot_location_from_NASIS_db()

# Fixed: 
# get_vegplot_textnote_from_NASIS_db()

# Relatively rare data  update soon with input from Jay
# get_vegplot_transpecies_from_NASIS_db()
