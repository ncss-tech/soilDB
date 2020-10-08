context("SDA_query() -- requires internet connection")


test_that("SDA_query() works", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }

  ## sample data

  # single-table result
  x.1 <<- suppressMessages(SDA_query(q = "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA630' ; "))

  # multi-table result
  x.2 <<- suppressMessages(SDA_query(q = "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA630'; SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA664' ;"))

  # table with multiple data types
  x.3 <<- suppressMessages(SDA_query(q = "SELECT TOP 100 mukey, cokey, compkind, comppct_r, majcompflag, elev_r, slope_r, wei, weg FROM component ;"))

  ## TODO: mukeys change through time, figure out a better way to query a known record
  # table with multi-line records
  x.4 <<- suppressMessages(SDA_query(q = "SELECT * from mutext WHERE mukey = '2596937';"))

  # point with known SSURGO data
  p <<- sp::SpatialPoints(cbind(-121.77100, 37.368402), proj4string = sp::CRS('+proj=longlat +datum=WGS84'))


  # standard request
  expect_true(inherits(x.1, 'data.frame'))
  expect_true(inherits(x.2, 'list'))

})

test_that("SDA_query() returns expected result", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # table dimensions
  expect_equal(nrow(x.1), 1)
  expect_equal(ncol(x.1), 2)

  # expected results
  x.12 <- do.call('rbind', x.2)
  expect_equal(x.1$areasymbol, 'CA630')
  expect_equal(x.12$areasymbol, c('CA630', 'CA664'))

})

test_that("SDA_query() SQL error / no results -> NULL", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # bad SQL should result in a warning and try-error result.
  expect_true(inherits(expect_warning(SDA_query("SELECT this from that")), 'try-error'))

  # queries that result in 0 rows should return NULL
  x <- suppressMessages(SDA_query("SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'xxx';"))
  expect_null(x)

})


test_that("SDA_spatialQuery() simple spatial query, tabular results", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  res <- suppressWarnings(SDA_spatialQuery(p, what = 'mukey'))

  # testing known values
  expect_true(inherits(res, 'data.frame'))
  expect_equal(nrow(res), 1)
  expect_match(res$muname, 'Diablo')

})


test_that("SDA_spatialQuery() simple spatial query, spatial results", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # test with default db = "SSURGO"
  res <- suppressWarnings(SDA_spatialQuery(p, what = 'geom'))

  # testing known values
  expect_true(inherits(res, 'SpatialPolygonsDataFrame'))
  expect_equal(nrow(res), 1)


  # test with db = "STATSGO"
  res <- suppressWarnings(SDA_spatialQuery(p, what = 'geom', db = "STATSGO"))

  # testing known values
  expect_true(inherits(res, 'SpatialPolygonsDataFrame'))
  expect_equal(nrow(res), 1)

})

test_that("SDA_query() interprets column names", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # x.3 is from the component table
  expect_equal(
    names(x.3),
    c("mukey", "cokey", "compkind", "comppct_r", "majcompflag", "elev_r", "slope_r", "wei", "weg")
    )

})



test_that("SDA_query() interprets data type correctly", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # x.3 is from the component table
  expect_true(inherits(x.3$mukey, 'integer'))
  expect_true(inherits(x.3$cokey, 'integer'))
  expect_true(inherits(x.3$compkind, 'character'))
  expect_true(inherits(x.3$comppct_r, 'integer'))
  expect_true(inherits(x.3$majcompflag, 'character'))
  expect_true(inherits(x.3$elev_r, 'integer'))
  expect_true(inherits(x.3$slope_r, 'integer'))
  expect_true(inherits(x.3$wei, 'integer'))
  expect_true(inherits(x.3$weg, 'integer'))

})



test_that("SDA_query() works with multi-line records", {

  skip_if_offline()

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }

  # https://github.com/ncss-tech/soilDB/issues/28
  expect_true(inherits(x.4, 'data.frame'))
  expect_true(nrow(x.4) == 6)

})




