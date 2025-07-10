context("SDA_query() -- requires internet connection")


test_that("SDA_query() works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

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
  
  skip_if_not_installed("sf")
  
  # point with known SSURGO data
  p <<- sf::st_as_sf(data.frame(x=-121.77100, y=37.368402),  coords=c("x","y"), crs = "EPSG:4326")
  
  skip_if(inherits(x.1, 'try-error'))
  
  skip_if(inherits(x.2, 'try-error'))
  
  # standard request
  expect_true(inherits(x.1, 'data.frame'))
  expect_true(inherits(x.2, 'list'))

})

test_that("SDA_query() returns expected result", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  skip_if(inherits(x.1, 'try-error'))
  
  skip_if(inherits(x.2, 'try-error'))
  
  # table dimensions
  expect_equivalent(nrow(x.1), 1)
  expect_equivalent(ncol(x.1), 2)

  # expected results
  x.12 <- do.call('rbind', x.2)
  expect_equivalent(x.1$areasymbol, 'CA630')
  expect_equivalent(x.12$areasymbol, c('CA630', 'CA664'))

})

test_that("SDA_query() query too long", {
  skip_if_not_installed("httr")
  
  q <- paste0("SELECT '1", paste0(rep(0, 1e7), collapse = ""), "' as a;")
  expect_error(soilDB::SDA_query(q), "Query string is too long")
})

test_that("SDA_query() SQL error / no results -> NULL", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # bad SQL should result in a warning and try-error result.
  expect_true(inherits(expect_message(SDA_query("SELECT this from that")), 'try-error'))

  # queries that result in 0 rows should return NULL
  x <- suppressMessages(SDA_query("SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'xxx';"))
  
  skip_if(inherits(x, 'try-error'))
  
  # if service is unavailable this returns try-error, not NULL
  expect_null(x)

})


test_that("SDA_spatialQuery() simple spatial query, tabular results", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # test first using an sf object converted internally to sp
  skip_if_not_installed("sf")
  
  if (requireNamespace("sf")) {
    res <- suppressWarnings(SDA_spatialQuery(sf::st_as_sf(p), what = 'mukey'))
    
    skip_if(inherits(res, 'try-error'))
    
    # testing known values
    expect_true(inherits(res, 'data.frame'))
    expect_equivalent(nrow(res), 1)
    expect_match(res$muname, 'Diablo')
  }
  
  # test with what = "sapolygon" 
  res <- suppressWarnings(SDA_spatialQuery(p, what = "areasymbol"))
  
  skip_if(inherits(res, 'try-error'))
  
  expect_true(inherits(res, 'data.frame'))
  expect_equivalent(nrow(res), 1)
  expect_match(res$areasymbol, 'CA641')
  
})


test_that("SDA_spatialQuery() simple spatial query, spatial results", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("sf")
  
  # test with default db = "SSURGO"
  res <- suppressWarnings(SDA_spatialQuery(p, what = 'geom'))
  
  skip_if(inherits(res, 'try-error'))
  
  # testing known values
  expect_true(inherits(res, 'sf'))
  expect_equivalent(nrow(res), 1)


  # test with db = "STATSGO"
  res <- suppressWarnings(SDA_spatialQuery(p, what = 'geom', db = "STATSGO"))
  
  skip_if(inherits(res, 'try-error'))
  
  # testing known values
  expect_true(inherits(res, 'sf'))
  expect_equivalent(nrow(res), 1)
  
  # test with what = "sapolygon" 
  res <- suppressWarnings(SDA_spatialQuery(p, what = "sapolygon"))
  
  skip_if(inherits(res, 'try-error'))
  
  # testing known values
  expect_true(inherits(res, 'sf'))
  expect_equivalent(nrow(res), 1)

})

test_that("SDA_spatialQuery() spatial query of MUKEY with multiple features", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if_not_installed("sf")
  
  x <- sf::st_as_sf(data.frame(x = c(-120, -120, -120), y = c(37, 37, 38)),
                coords = c('x', 'y'), crs = 4326)
  
  res <- SDA_spatialQuery(x)
  
  # if the result set is empty rather than an error, SDA_query() result can be NULL
  skip_if(is.null(res) || inherits(res, 'try-error'))
  
  expect_equivalent(nrow(res), 2)
  
  res2 <- SDA_spatialQuery(x, db = "STATSGO")
  expect_equivalent(nrow(res), 2)
})

test_that("SDA_query() interprets column names", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  skip_if(inherits(x.3, 'try-error'))
  
  # x.3 is from the component table
  expect_named(x.3, c("mukey", "cokey", "compkind", "comppct_r", "majcompflag", "elev_r", "slope_r", "wei", "weg"))

})



test_that("SDA_query() interprets data type correctly", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  skip_if(inherits(x.3, 'try-error'))
  
  # x.3 is from the component table
  expect_true(inherits(x.3$mukey, 'integer'))
  expect_true(inherits(x.3$cokey, 'integer'))
  expect_true(inherits(x.3$compkind, 'character'))
  expect_true(inherits(x.3$comppct_r, 'integer'))
  expect_true(inherits(x.3$majcompflag, 'character'))
  expect_true(inherits(x.3$elev_r, 'numeric'))
  expect_true(inherits(x.3$slope_r, 'numeric'))
  expect_true(inherits(x.3$wei, 'character'))
  expect_true(inherits(x.3$weg, 'character'))

})

test_that("SDA_query() works with multi-line records", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  skip_if(inherits(x.4, 'try-error'))
  
  # https://github.com/ncss-tech/soilDB/issues/28
  expect_true(inherits(x.4, 'data.frame'))
  expect_true(nrow(x.4) == 7)

})




