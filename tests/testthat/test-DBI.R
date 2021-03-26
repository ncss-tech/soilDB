# context("test DBI conversion")
# 
# test_that('tests for NA values (DBI/odbc replacement of RODBC)', {
#   
#   # test for conditions permitting this test to run
#   if (!local_NASIS_defined(dsn = NULL)) {
#     skip("local NASIS database not available")
#   }
# 
#   # rodbccontest <- RODBC::odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials'))
#   # 
#   # this works as expected
#   # expect_equal(
#   #   RODBC::sqlQuery(
#   #     rodbccontest,
#   #     "SELECT aspect FROM site_View_1"
#   #   )$aspect,
#   #   RODBC::sqlQuery(
#   #     rodbccontest,
#   #     "SELECT * FROM site_View_1"
#   #   )$aspect
#   # )
#   
#   # built in "whole table" query is equivalent to RODBC
#   # expect_equal(
#   #   dbQueryNASIS(NASIS(), "SELECT * FROM site")$aspect,
#   #   RODBC::sqlQuery(rodbccontest, "SELECT * FROM site")$aspect
#   # )
#   # 
#   # expect_equal(
#   #   soilDB:::.dump_NASIS_table("site")$aspect,
#   #   RODBC::sqlQuery(rodbccontest, "SELECT * FROM site")$aspect
#   # )
#   # 
#   # RODBC::odbcClose(rodbccontest)
# })
