test_that("soilDBdata: fetchNASIS", {

  skip_on_cran()

  # soilDBdata package is available on GitHub and provides SQLite data sources with real data
  skip_if_not_installed("soilDBdata")

  PEDON_TEST_DSN <- system.file("extdata/fetchNASIS_pedons.sqlite", package = "soilDBdata")[1]
  COMPONENT_TEST_DSN <- system.file("extdata/fetchNASIS_components.sqlite", package = "soilDBdata")[1]

  # test SS=FALSE
  fp <- fetchNASIS(from = "pedons", dsn = PEDON_TEST_DSN, SS = FALSE)
  
  fc <- fetchNASIS(from = "components", dsn = COMPONENT_TEST_DSN, SS = FALSE)

  # TODO: test SS=TRUE
  # TODO: test rmHzErrors

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
