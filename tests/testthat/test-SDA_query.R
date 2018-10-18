context("SDA_query() -- requires internet connection")


## sample data

# single-table result
x.1 <- suppressMessages(SDA_query(q = "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA630' ; "))

# multi-table result
x.2 <- suppressMessages(SDA_query(q = "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA630'; SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA664' ;"))

# point with known SSURGO data
p <- sp::SpatialPoints(cbind(-121.77100, 37.368402), proj4string = sp::CRS('+proj=longlat +datum=WGS84'))

## tests

test_that("SDA_query() returns a data.frame or list", {
  
  # standard request
  expect_match(class(x.1), 'data.frame')
  expect_match(class(x.2), 'list')

})

test_that("SDA_query() returns expected result", {
  
  # table dimensions
  expect_equal(nrow(x.1), 1)
  expect_equal(ncol(x.1), 2)
  
  # expected results
  x.12 <- do.call('rbind', x.2)
  expect_equal(x.1$areasymbol, 'CA630')
  expect_equal(x.12$areasymbol, c('CA630', 'CA664'))
  
})

test_that("SDA_query() SQL error / no results -> NULL", {
  
  # bad SQL should result in a local error
  expect_error(SDA_query("SELECT this from that"))
  
  # queries that result in 0 rows should return NULL
  x <- suppressMessages(SDA_query("SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'xxx';"))
  expect_null(x)
  
})


test_that("SDA_spatialQuery() simple spatial query, tabular results", {
  
  res <- SDA_spatialQuery(p, what = 'mukey')
  
  # testing known values
  expect_match(class(res), 'data.frame')
  expect_equal(nrow(res), 1)
  expect_match(res$muname, 'Diablo')
  
})


test_that("SDA_spatialQuery() simple spatial query, spatial results", {

    res <- SDA_spatialQuery(p, what = 'geom')
  
  # testing known values
  expect_match(class(res), 'SpatialPolygonsDataFrame')
  expect_equal(nrow(res), 1)
  
})




