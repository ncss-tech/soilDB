context("fetchSoilGrids() -- requires internet connection")

test_that("fetchSoilGrids() works as expected", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if(as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)))

  library(aqp)
  
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

  expect_equivalent(nrow(aqp::site(res)), 2)
  expect_equivalent(aqp::nrow(res), 2 * 6)
  
  # 5 base columns in SPC: label, id, hzdept, hzdepb, hzID
  #   plus 13 default variables w/ 5 columns each 
  expect_equivalent(ncol(aqp::horizons(res)), 5 + (13 * 5))

  # data conversion works
  expect_true(res$claymean[1] > 0 && res$claymean[1] < 100)
  
  # bogus loc.names
  expect_error( {res <- fetchSoilGrids(your.points, loc.names = "foo")} )

})
