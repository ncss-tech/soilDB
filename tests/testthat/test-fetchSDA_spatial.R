context("fetchSDA_spatial() -- requires internet connection")

test_that("fetchSDA_spatial() works", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # expect 3, relatively nonextensive join delineations
  single.mukey <- fetchSDA_spatial(x = "2924882")
  expect_equal(nrow(single.mukey), 3)
  
  # expect 48 delineations for whole nmusymn
  full.extent.nmusym <- fetchSDA_spatial(x = "2x8l5", by = "nmusym")
  expect_equal(nrow(full.extent.nmusym), 144)
  
  # mukey value from single result is in full extent result
  expect_true(unique(single.mukey$mukey) %in% unique(full.extent.nmusym$mukey))

  # make sure additional fields get into result
  withname <- fetchSDA_spatial(x = "2x8l5", by="nmusym", add.fields="muname")
  expect_true(all(withname$muname == "Pentz-Bellota complex, 2 to 15 percent slopes"))
  
})
