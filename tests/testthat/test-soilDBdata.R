test_that("soilDBdata: fetchNASIS", {

  skip_on_cran()

  # soilDBdata package is available on GitHub and provides SQLite data sources with real data
  skip_if_not_installed("soilDBdata")

  PEDON_TEST_DSN <- system.file("extdata", "fetchNASIS_pedons.sqlite", package = "soilDBdata")[1]
  COMPONENT_TEST_DSN <- system.file("extdata", "fetchNASIS_components.sqlite", package = "soilDBdata")[1]

  # test SS=FALSE
  fp <- fetchNASIS(from = "pedons", dsn = PEDON_TEST_DSN, SS = FALSE)
  
  fc <- fetchNASIS(from = "components", dsn = COMPONENT_TEST_DSN, SS = FALSE)

  # test SS=TRUE
  fp2 <- fetchNASIS(from = "pedons", dsn = PEDON_TEST_DSN, SS = TRUE)
  
  fc2 <- fetchNASIS(from = "components", dsn = COMPONENT_TEST_DSN, SS = TRUE)
  
  # test rmHzErrors
  fp3 <- fetchNASIS(from = "pedons", dsn = PEDON_TEST_DSN, SS = FALSE, rmHzErrors = TRUE)

  # test fill=TRUE
  expect_warning({
    fpf <- fetchNASIS(
      from = "pedons",
      dsn = PEDON_TEST_DSN,
      SS = FALSE,
      fill = TRUE
    )
  })
  
  p1 <- aqp::subset(fpf, peiid == 1092666)
  
  # check surface fragments
  expect_equal(p1$surface_stones, 2.4)
  expect_equal(p1$surface_total_frags_pct, 5.4)
  
  # check subsurface fragments 
  expect_equal(p1$stones, rep(5, 4))
  expect_equal(p1$total_frags_pct, c(90, 75, 65, 75))
  
  expect_warning({
    fcf <- fetchNASIS(
      from = "components",
      dsn = COMPONENT_TEST_DSN,
      SS = FALSE,
      fill = TRUE
    )
  })

  expect_true(all(sapply(list(fp, fc, fpf, fcf), inherits, 'SoilProfileCollection')))

})

test_that("soilDBdata: fetchVegdata", {
  
  skip_on_cran()
  
  # soilDBdata package is available on GitHub and provides SQLite data sources with real data
  skip_if_not_installed("soilDBdata")
  
  VEGPLOT_TEST_DSN <- system.file("extdata", "fetchVegdata.sqlite", package = "soilDBdata")[1]
  
  # test SS=FALSE
  fvp1 <- fetchVegdata(dsn = VEGPLOT_TEST_DSN, SS = FALSE)
  
  expect_true(all(sapply(fvp1, inherits, 'data.frame')))
  
  # only one transect missing points/species info
  expect_equal(sum(!is.na(fvp1$vegtranspoint$plantsym)), nrow(fvp1$vegtranspoint) - 1)

  # TEST SS=TRUE
  fvp2 <- fetchVegdata(dsn = VEGPLOT_TEST_DSN, SS = TRUE)
  
  # test include_pedon = "assocuserpedonid"
  fvp3 <- fetchVegdata(dsn = VEGPLOT_TEST_DSN, SS = FALSE, include_pedon = "assocuserpedonid")
  
  # test include_pedon = FALSE
  fvp4 <- fetchVegdata(dsn = VEGPLOT_TEST_DSN, SS = FALSE, include_pedon = FALSE)
  
  # include_pedon = "assocuserpedonid":
  #   more vegplots have pedon linked implicitly via shared site observation record
  #   fewer vegplots have pedon linked explicitly via "assocuserpedonid"
  expect_true(sum(!is.na(fvp1$vegplot$peiid)) >= sum(!is.na(fvp3$vegplot$peiid)))

  # include_pedon = FALSE: no peiid/linkage to pedon table via peiid
  expect_true(is.character(fvp4$vegplot$assocuserpedonid))
  expect_null(fvp4$vegplot$peiid)
})
