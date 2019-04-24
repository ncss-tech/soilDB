context("fetchSDA_component() -- requires internet connection")

## sample data: these should run in < 3 seconds

# single component
x <- suppressMessages(fetchSDA_component(WHERE="nationalmusym = 'kzc4'"))


## tests

test_that("fetchSDA_component() returns an SPC", {
  
  # SPC integrity and expected IDs / hz depths
  expect_match(class(x), 'SoilProfileCollection')
  expect_equal(idname(x), 'cokey')
  expect_equal(horizonDepths(x), c('hzdept_r', 'hzdepb_r'))
  
})

test_that("fetchSDA_component() returns expected results", {
  
  # there should be 2 components nad 10 horizons
  expect_equal(length(x), 2)
  expect_equal(nrow(x), 10)
  
  # should match original WHERE clause
  expect_equal(x$nationalmusym, c('kzc4', 'kzc4'))
  expect_equal(site(x)$nationalmusym, c('kzc4', 'kzc4'))
  
})
