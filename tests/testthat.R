library(testthat)
library(soilDB)


# set to FALSE for submissions to CRAN
# set to TRUE for testing non-NASIS components on machines that don't have local ODBC connection setup
options(.soilDB_testNetworkFunctions=TRUE)


test_check("soilDB")
