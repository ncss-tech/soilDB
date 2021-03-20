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

# path to data source (NULL = use ODBC to local nasis,
#                     otherwise path to SQLite)
dsn <- NULL # "~/workspace/NASISlite/nasis_local.db" # "misc/testStatic.sqlite"

test_local_NASIS <- function(SS = FALSE, static_path = NULL) {

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
        switch (FUN,
                "local_NASIS_defined" = try(TESTFUN(static_path = static_path)),
                try(TESTFUN(SS = SS, static_path = static_path)) )
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
res <- test_local_NASIS(SS = FALSE, static_path = NULL)

# list names of failed functions; if length 0 all good
res[which(res)]

# test against whole local database
res <- test_local_NASIS(SS = FALSE, static_path = NULL)

res[which(res)]

# RUN IF NEEDED:
# createStaticNASIS(static_path = NULL, SS = TRUE, output_path = dsn)

# test with selected set in SQLite instance
res <- test_local_NASIS(SS = TRUE, static_path = dsn)

res[which(res)]

# test against whole local database in SQLite instance
res <- test_local_NASIS(SS = FALSE, static_path = dsn)

res[which(res)]


### prior fixes:

# Fixed: Text fields must come at end of query per MSSQL specs
# get_text_notes_from_NASIS_db()

# Fixed: same as above
# get_vegplot_location_from_NASIS_db()

# Fixed:
# get_vegplot_textnote_from_NASIS_db()

# Relatively rare data  update soon with input from Jay
# get_vegplot_transpecies_from_NASIS_db()
