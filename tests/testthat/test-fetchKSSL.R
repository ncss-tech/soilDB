context("fetchKSSL() -- requires internet connection")


## sample data
x <- fetchKSSL(series='sierra')
x.morph <- fetchKSSL(series='sierra', returnMorphologicData = TRUE)

test_that("fetchKSSL() returns an SPC or list", {
  
  # standard request
  expect_match(class(x), 'SoilProfileCollection')
  
  # SPC + morphologic data
  expect_match(class(x.morph), 'list')
  expect_match(class(x.morph$SPC), 'SoilProfileCollection')
  expect_match(class(x.morph$morph), 'list')
  
})


test_that("fetchKSSL() returns reasonable data", {
  
  # standard request
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'pedon_key')
  expect_equal(horizonDepths(x), c("hzn_top", "hzn_bot"))
  
})

test_that("fetchKSSL() returns data associated with named series (sierra)", {
  
  # all of the results should contain the search term
  f <- grepl('sierra', x$taxonname, ignore.case = TRUE)
  expect_equal(all(f), TRUE)
  
})


test_that("fetchKSSL() returns NULL with bogus query", {
  
  # all of the results should contain the search term
  res <- suppressMessages(fetchKSSL(series='XXX'))
  expect_null(res)
  
})
