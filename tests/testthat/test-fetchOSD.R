context("fetchOSD() -- requires internet connection")


test_that("fetchOSD() works", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if(! soilDB:::.local_NASIS_defined()) {
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
  if(! soilDB:::.local_NASIS_defined()) {
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
  if(! soilDB:::.local_NASIS_defined()) {
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
  if(! soilDB:::.local_NASIS_defined()) {
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
  if(! soilDB:::.local_NASIS_defined()) {
    skip("in-house testing only")
  }
  
  # extended request
  expect_equal(
    names(x.extended), 
    c("SPC", "competing", "geomcomp", "hillpos", "mtnpos", "pmkind", "pmorigin", "mlra", "climate.annual", "climate.monthly", "soilweb.metadata")
    )
  
})

test_that("fetchOSD() returns data associated with named series (sierra|cecil)", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if(! soilDB:::.local_NASIS_defined()) {
    skip("in-house testing only")
  }
  
  # all of the results should contain the search term
  f <- grepl('sierra|cecil', x$id, ignore.case = TRUE)
  expect_equal(all(f), TRUE)
  
})

