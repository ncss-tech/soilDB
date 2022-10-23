test_that("get_SRI() works", {

  skip_if_offline()

  skip_on_cran()

  sri_deschutes <- get_SRI(gdb = 'Deschutes')

  expect_equal(nrow(sri_deschutes), 4114)

})

test_that("get_SRI_layers() works", {

  skip_if_offline()

  skip_on_cran()

  sri_layers <- suppressWarnings(get_SRI_layers(gdb =  'Winema'))

  expect_equal(lengths(sri_layers)[[1]], 11)

})

