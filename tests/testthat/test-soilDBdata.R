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

  # test fill=TRUE
  expect_warning({
    fpf <- fetchNASIS(
      from = "pedons",
      dsn = PEDON_TEST_DSN,
      SS = FALSE,
      fill = TRUE
    )
  })
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
