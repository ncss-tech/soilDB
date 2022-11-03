test_that("get_SRI() works", {

  skip_if_offline()

  skip_on_cran()

  sri_deschutes <- get_SRI(gdb = 'Deschutes')

  expect_equal(nrow(sri_deschutes), 4114)

  # multiple layers

  sri_deschutes_multiple <- get_SRI(gdb = 'Deschutes', layers = c('MapUnits', 'ErosionAndHydro', 'SampleSites_MaterialsTesting'), quiet = T)

  expect_equal(length(sri_deschutes_multiple), 3)

  # short name

  sri_deschutes_multiple <- get_SRI(gdb = 'desc', layers = c('MapUnits', 'ErosionAndHydro', 'SampleSites_MaterialsTesting'), quiet = T)

  expect_equal(length(sri_deschutes_multiple), 3)

})

test_that("get_SRI_layers() works", {

  skip_if_offline()

  skip_on_cran()

  sri_layers <- get_SRI_layers(gdb =  'Winema')

  expect_equal(nrow(sri_layers), 11)

  # short name

  sri_layers <- get_SRI_layers(gdb = 'win')

  expect_equal(nrow(sri_layers), 11)

})

