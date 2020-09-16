context("fetchOSD() -- requires internet connection")

## these are the elements of the list returned when extended=TRUE
## update here as-needed
extended.table.names <<- c("SPC", "competing", "geomcomp", "hillpos", "mtnpos", "terrace", "flats", "pmkind", "pmorigin", "mlra", "climate.annual", "climate.monthly", "soilweb.metadata")

test_that("fetchOSD() works", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  ## sample data
  x <<- fetchOSD(soils = c('sierra', 'cecil'))
  x.extended <<- fetchOSD(soils = c('sierra', 'cecil'), extended = TRUE)
  
  # standard request
  expect_true(inherits(x, 'SoilProfileCollection'))
  
})



test_that("fetchOSD() returns NULL with bogus query", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchOSD(soils='XXX'))
  expect_null(res)
  
})



test_that("fetchOSD() returns a list + SPC in extended mode", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # extended request
  expect_true(inherits(x.extended, 'list'))
  expect_true(inherits(x.extended$SPC, 'SoilProfileCollection'))
  
})




test_that("fetchOSD() returns reasonable data", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # standard request
  expect_equal(nrow(site(x)) == 2, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'id')
  expect_equal(horizonDepths(x), c("top", "bottom"))
  
})


test_that("fetchOSD() returns reasonable data in extended mode", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # extended request
  expect_equal(
    names(x.extended), extended.table.names
    )
  
})


# https://github.com/ncss-tech/soilDB/issues/128
test_that("fetchOSD() returns warning in extended mode, when active + inactive series specified", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # warning added in response to #128
  expect_warning(
    xx <- fetchOSD(soils = c('sierra', 'Breadsprings', 'Hagerwest', 'Tintero'), extended = TRUE)
  )
  
  # extended request components
  expect_equal(
    names(xx), extended.table.names
  )
  
})


test_that("fetchOSD() returns data associated with named series (sierra|cecil)", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # all of the results should contain the search term
  f <- grepl('sierra|cecil', x$id, ignore.case = TRUE)
  expect_equal(all(f), TRUE)
  
})

