context("fetchSDA_spatial -- requires internet connection")

test_that("fetchSDA_spatial basic mupolygon functionality", {

  skip_if_offline()

  skip_on_cran()

  # expect 3, relatively non-extensive join delineations
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

test_that("fetchSDA_spatial sapolygon and gsmmupolygon", {

  skip_if_offline()

  skip_on_cran()

  # test SSA geometry sapolygon

  symbs <- c('CA077','CA632','CA644','CA630', 'CA628', 'CA649')

  by.areasym.bbox <- fetchSDA_spatial(x = symbs,
                                      # by.col = "areasymbol", # implied by datatype of x and geom.src
                                      method = "bbox",
                                      geom.src = "sapolygon",
                                      add.fields = "legend.areaname")
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

  expect_true(all(by.areasym.pt$areaname %in% c("Amador Area, California",
                                         "Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties",
                                         "San Joaquin County, California",
                                         "Eastern Stanislaus Area, California",
                                         "Mariposa County Area, California",
                                         "Stanislaus County, California, Northern Part")))

  # test STATSGO mupolygon
  statsgo.bbox <- fetchSDA_spatial(660848, db = 'STATSGO', method = "bbox",
                                   add.fields = c("mapunit.muname","legend.areaname"))
  expect_equal(nrow(statsgo.bbox), 5)

  # skip_if_not_installed('sf')
  # suppressWarnings(requireNamespace("sf"))
  # suppressWarnings(requireNamespace("lwgeom"))
  # # test CLIPAREASYMBOL
  # an_extent <- fetchSDA_spatial(x = 660972, db = 'STATSGO', chunk.size = 1, add.fields = "legend.areaname")
  # tst <- sf::st_as_sf(an_extent)
  # expect_equal(nrow(an_extent), 7)
  # expect_true(all.equal(sum(sf::st_area(tst)), sf::st_area(sf::st_union(tst))))
})

