context("fetchKSSL() -- requires internet connection")

library(aqp)

## sample data
x <- fetchKSSL(series='sierra')
x.morph <- fetchKSSL(series='sierra', returnMorphologicData = TRUE)
x.morp.simple.colors <- fetchKSSL(series='sierra', returnMorphologicData = TRUE, simplifyColors = TRUE)

test_that("fetchKSSL() returns an SPC or list", {
  
  # standard request
  expect_match(class(x), 'SoilProfileCollection')
  
  # SPC + morphologic data
  expect_match(class(x.morph), 'list')
  expect_match(class(x.morph$SPC), 'SoilProfileCollection')
  expect_match(class(x.morph$morph), 'list')
  
  # simplified colors, merges into @horizons
  expect_false(is.null(x.morp.simple.colors$SPC$moist_soil_color))
  
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
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchKSSL(series='XXX'))
  expect_null(res)
  
})


test_that("fetchKSSL() fails gracefully when morphology data are missing", {
  
  # pedon_key 37457 is missing:
  # * most lab data
  # * all morphologic data
  # --> cannot simplify colors, so skip
  res <- suppressMessages(fetchKSSL(pedon_key=37457, returnMorphologicData = TRUE, simplifyColors = TRUE))
  expect_false(res$morph$phcolor)
  expect_false(res$morph$phfrags)
  expect_false(res$morph$phpores)
  expect_false(res$morph$phstructure)
  expect_false(res$morph$pediagfeatures)
  
})

