## validation of NASIS-functionality for nasisDBI PR
## git shell command to list files changed in the PR

# git diff --name-only master...nasisDBI | grep -E get\\|NASIS\\|fetch

# for vegplot data that are complete
#  - VEG - Veg Plots by User Site ID and Vegplot ID for %WY629% 

f <- read.table(text = "R/fetchNASIS.R
                        R/fetchNASIS_pedons.R
                        R/fetchNASIS_components.R
                        R/fetchNASISLabData.R
                        R/fetchVegdata.R
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
                        R/get_soilseries_from_NASIS.R
                        R/get_text_notes_from_NASIS_db.R
                        R/get_veg_data_from_NASIS_db.R
                        R/get_vegplot_data_from_NASIS_db.R
                        R/openNASISchannel.R")$V1

# # you want the version of soilDB you are testing to be installed
# devtools::install()

library(soilDB)

# path to data source (NULL = use ODBC to local nasis,
#                     otherwise path to SQLite)
dsn <- "misc/testStatic.sqlite"
# 
# # RUN IF NEEDED:
# # dsn <- "misc/testStatic.sqlite"
# createStaticNASIS(tables = c(get_NASIS_table_name_by_purpose(SS = TRUE),
#                              get_NASIS_table_name_by_purpose(SS = FALSE)),
#                   dsn = NULL, output_path = dsn)

# Function to load all function names in package, run them using SS and dsn as specified
test_local_NASIS <- function(SS = FALSE, dsn = NULL) {

  # get package function names
  fnames <- sapply(f, function(x) {
    names(as.list(evalSource(x, package = "soilDB")))
  })

  # iterate over functions by name
  test <- lapply(fnames, function(fname) {

      lapply(fname, function(FUN) {

        message("\n")
        message(sprintf("Testing: %s", FUN))
        message("\n")

        # get function out of (installed) soilDB package environment
        TESTFUN <- get(FUN, envir = as.environment("package:soilDB"))

        # handle special cases -- all functions tested take an SS argument except local_NASIS_defined
        switch(FUN,
               "local_NASIS_defined" = try(TESTFUN(dsn = dsn)),
               "get_soilseries_from_NASIS" = try(TESTFUN(dsn = dsn)),
               "get_soilseries_from_NASISWebReport" = NULL, 
               try(TESTFUN(SS = SS, dsn = dsn)) )
      })
    })

  # which functions error? that is the function result -- in addition to whatever messages/out generated
  unlist(lapply(names(test), function(x) lapply(seq_along(test[[x]]), function(y) {
        res <- inherits(test[[x]][[y]], 'try-error')
        names(res) <- fnames[[x]][y]
        res
      })))
}

# test with selected set
nasis_ss <- test_local_NASIS(SS = TRUE, dsn = NULL)

# list names of failed functions; if length 0 all good
nasis_ss[which(nasis_ss)]

# test against whole local database
nasis_all <- test_local_NASIS(SS = FALSE, dsn = NULL)

nasis_all[which(nasis_all)]

# test with selected set in SQLite instance
nasis_static_ss <- test_local_NASIS(SS = TRUE, dsn = dsn)

nasis_static_ss[which(nasis_static_ss)]

# get_soilseries_from_NASIS(dsn = dsn)

# test against whole local database in SQLite instance
nasis_static_all <- test_local_NASIS(SS = FALSE, dsn = dsn)

nasis_static_all[which(nasis_static_all)]

save(nasis_ss, nasis_all, nasis_static_ss, nasis_static_all, 
     file = "NASIS-table-results.rda")

### prior fixes:

# Fixed: Text fields must come at end of query per MSSQL specs
# get_text_notes_from_NASIS_db()

# Fixed: same as above
# get_vegplot_location_from_NASIS_db()

# Fixed:
# get_vegplot_textnote_from_NASIS_db()

# Relatively rare data  update soon with input from Jay
# get_vegplot_transpecies_from_NASIS_db()
