dsn <- NULL

test_that("dbQueryNASIS works", {
  
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }
  
  # making a DBIConnection works
  conn <- dbConnectNASIS()
  expect_true(inherits(conn, 'DBIConnection'))
  
  # a single query works
  res <- dbQueryNASIS(conn, "SELECT geomftname, geomftiid FROM geomorfeattype", close = FALSE)
  
  expect_true(inherits(res, 'data.frame') && 
                all(c("geomftname", "geomftiid") %in% colnames(res)) &&
                res$geomftname[1] == "Landform")
  
  # a double query works (returns a named list)
  res2 <- dbQueryNASIS(conn, q = c(geomorfeat = "SELECT geomfname, geomfiid, geomftiidref FROM geomorfeat",
                                   geomorfeattype = "SELECT geomftname, geomftiid FROM geomorfeattype"))
  
  expect_true(is.list(res2) && 
                all(c("geomorfeat", "geomorfeattype") %in% names(res2)))
  
  # results for geomorfeattype should be identical
  expect_equal(res, res2[["geomorfeattype"]])
  
  # connection is closed from close=TRUE from end of res2 call
  expect_error(dbQueryNASIS(conn, "SELECT geomftname, geomftiid FROM geomorfeattype"))
})
