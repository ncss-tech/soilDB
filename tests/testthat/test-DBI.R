context("test DBI conversion")

test_that('tests for NA values (DBI/odbc replacement of RODBC)', {
  
  # test for conditions permitting this test to run
  if (!local_NASIS_defined(dsn = NULL)) {
    skip("local NASIS database not available")
  }
  
  # hypothesis: this DBI NA mangling is related to ODBC 13 * nanodbc interaction
  #             note: exercising this test requires site records without aspect populated in local db
  if (sum(is.na(dbQueryNASIS(NASIS(), "SELECT aspect FROM site")$aspect)) > 0) {
    # NA filled with (usually large) integer values--much greater than 360 implied by domain
    expect_equal(sum(is.na(
      dbQueryNASIS(NASIS(), "SELECT * FROM site")$aspect
    )), 0)
  } else {
    skip("no missing aspect values in site table")
  }
  
  rodbccontest <- RODBC::odbcDriverConnect(connection = getOption('soilDB.NASIS.credentials'))
  
  # this works as expected
  # expect_equal(
  #   RODBC::sqlQuery(
  #     rodbccontest,
  #     "SELECT aspect FROM site_View_1"
  #   )$aspect,
  #   RODBC::sqlQuery(
  #     rodbccontest,
  #     "SELECT * FROM site_View_1"
  #   )$aspect
  # )
  
  # built in "whole table" query is equivalent to RODBC
  expect_equal(
    soilDB:::.dump_NASIS_table("site_View_1")$aspect,
    RODBC::sqlQuery(rodbccontest, "SELECT * FROM site_View_1")$aspect
  )
  
  RODBC::odbcClose(rodbccontest)
})
