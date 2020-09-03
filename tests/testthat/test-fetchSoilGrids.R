context("fetchSoilGrids() -- requires internet connection")

test_that("fetchSoilGrids() works as expected", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if (!getOption('.soilDB_testNetworkFunctions'))  {
    skip("in-house testing only")
  }
  
  your.points <- data.frame(id = c("A", "B"), 
                            lat = c(37.9, 38.1), 
                            lon = c(-120.3, -121.5), 
                            stringsAsFactors = FALSE)
                           
  res <- fetchSoilGrids(your.points)
  
  # contents as expected
  expect_true(aqp::spc_in_sync(res)$valid)
  expect_true(all(aqp::checkHzDepthLogic(res)$valid))
  
  expect_equal(length(res), 2)
  expect_equal(nrow(res),   2*6)
  
  expect_equal(ncol(site(res)), 1)
  expect_equal(ncol(horizons(res)), 2*25)
  
  # data conversion works
  expect_equal(res$claymean[1], 18.7)
  
  # default names kick in
  expect_silent( {res <- fetchSoilGrids(your.points, loc.names = NULL)} )
  
  # bogus loc.names
  expect_error( {res <- fetchSoilGrids(your.points, loc.names = "foo")} )
  
  # custom column names
  colnames(your.points) <- letters[1:3]
  expect_error( {res <- fetchSoilGrids(your.points, loc.names = "foo")} )
  expect_silent( {res <- fetchSoilGrids(your.points, loc.names = letters[1:3])} )
})
