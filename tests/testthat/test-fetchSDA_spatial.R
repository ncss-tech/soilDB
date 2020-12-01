context("fetchSDA_spatial -- requires internet connection")

test_that("fetchSDA_spatial basic mupolygon functionality", {
  
  skip_if_offline()
  
  if (!getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # expect 3, relatively nonextensive join delineations
  single.mukey <- fetchSDA_spatial(x = "2924882", by.col = 'mukey')
  expect_equal(nrow(single.mukey), 3)
  
  # there are currently 3 MUKEY associated with this national musym
  # expect 48 delineations for this nmusymn
  # also test verbose argument
  expect_silent({full.extent.nmusym <- fetchSDA_spatial(x = "2x8l5", by.col = "nmusym", verbose = FALSE)})
  expect_equal(nrow(full.extent.nmusym), 48)
  
  # mukey value from single result is in full extent result
  expect_true(unique(single.mukey$mukey) %in% unique(full.extent.nmusym$mukey))

  # make sure additional fields get into result
  withname <- fetchSDA_spatial(x = "2x8l5", by.col = "nmusym", add.fields = "muname")
  expect_true(all(withname$muname == "Pentz-Bellota complex, 2 to 15 percent slopes"))
  
})

test_that("fetchSDA_spatial sapolygon extensions", {
  
  skip_if_offline()
  
  if (!getOption('.soilDB_testNetworkFunctions'))  {
    skip("in-house testing only")
  }
  
  # test SSA geometry
  
  symbols <- c('CA077','CA632','CA644','CA630', 'CA628', 'CA649')
  
  by.areasym.bbox <- fetchSDA_spatial(x = symbols, 
                                      # by.col = "areasymbol", # implied by datatype of x and geom.src
                                      method = "bbox",
                                      geom.src = "sapolygon")
  expect_equal(nrow(by.areasym.bbox), 6)
  
  ## this one is slow
  # by.areasym <- fetchSDA_spatial(x = unique(by.areasym.bbox$lkey),
  #                                by.col = "areasymbol",
  #                                geom.src = "sapolygon")
  
  by.areasym.pt <- fetchSDA_spatial(x = unique(by.areasym.bbox$lkey),
                                    # by.col = "lkey", # implied by datatype of x and geom.src
                                    method = "point",
                                    geom.src = "sapolygon",
                                    add.fields = "legend.areaname")
  
  expect_equal(by.areasym.pt$areaname, c("Amador Area, California", 
                                         "Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties", 
                                         "San Joaquin County, California", 
                                         "Eastern Stanislaus Area, California", 
                                         "Mariposa County Area, California", 
                                         "Stanislaus County, California, Northern Part"))
})
