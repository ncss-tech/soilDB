context("fetchSoilGrids() -- requires internet connection")

test_that("fetchSoilGrids() works as expected", {

  skip_if_offline()

  skip_on_cran()

  your.points <- data.frame(id = c("A", "B"),
                            lat = c(37.9, 38.1),
                            lon = c(-120.3, -121.5),
                            stringsAsFactors = FALSE)

  res <- try(fetchSoilGrids(your.points), silent = TRUE)
  
  if (inherits(res, 'try-error'))
    skip("SoilGrids API not accessible")

  # contents as expected
  expect_true(aqp::spc_in_sync(res)$valid)
  expect_true(all(aqp::checkHzDepthLogic(res)$valid))

  expect_equal(length(res), 2)
  expect_equal(nrow(res),   2*6)

  # 5 base columns in SPC: label, id, hzdept, hzdepb, hzID
  #   plus 10 default variables w/ 5 columns each 
  expect_equal(ncol(horizons(res)), 5 + (13 * 5))

  # data conversion works
  expect_equal(res$claymean[1], 18.7)
  
  skip_if(as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)))
  
  # default names kick in
  expect_silent( {res <- fetchSoilGrids(your.points, loc.names = NULL)} )

  # bogus loc.names
  expect_error( {res <- fetchSoilGrids(your.points, loc.names = "foo")} )

  # custom column names
  colnames(your.points) <- letters[1:3]
  expect_error( {res <- fetchSoilGrids(your.points, loc.names = "foo")} )
  expect_silent( {res <- fetchSoilGrids(your.points, loc.names = letters[1:3])} )
})

# test_that("fetchSoilGrids: POINT input", {
#   
#   skip_if_offline()
#   
#   skip_on_cran()
#   
#   skip_if_not_installed("sf")
#   
#   your.points <- data.frame(siteid = c("A", "B"),
#                             lat = c(37.9, 38.1),
#                             lon = c(-120.3, -121.5),
#                             stringsAsFactors = FALSE)
#   your.points <- sf::st_as_sf(your.points, coords = c('lon','lat'))
#   res <- try(fetchSoilGrids(your.points), silent = TRUE)
#   
#   expect_true(inherits(res, 'SoilProfileCollection'))
# })
