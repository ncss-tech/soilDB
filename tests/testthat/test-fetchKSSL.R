context("fetchKSSL() -- requires internet connection")

test_that("fetchKSSL() works", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  ## sample data
  x <<- fetchKSSL(series='sierra')
  x.morph <<- fetchKSSL(series='sierra', returnMorphologicData = TRUE)
  x.morp.simple.colors <<- fetchKSSL(series='sierra', returnMorphologicData = TRUE, simplifyColors = TRUE)
  
  # standard request
  expect_true(inherits(x, 'SoilProfileCollection'))
  
  
})


test_that("fetchKSSL() returns an SPC or list", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # SPC + morphologic data
  expect_true(inherits(x.morph, 'list'))
  expect_true(inherits(x.morph$SPC, 'SoilProfileCollection'))
  expect_true(inherits(x.morph$morph, 'list'))
  
  # simplified colors, merges into @horizons
  expect_false(is.null(x.morp.simple.colors$SPC$moist_soil_color))
  
})


test_that("fetchKSSL() returns reasonable data", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # standard request
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'pedon_key')
  expect_equal(horizonDepths(x), c("hzn_top", "hzn_bot"))
  
})

test_that("fetchKSSL() returns data associated with named series (sierra)", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # all of the results should contain the search term
  f <- grepl('sierra', x$taxonname, ignore.case = TRUE)
  expect_equal(all(f), TRUE)
  
})


test_that("fetchKSSL() returns data associated with multiple named series", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  x.multiple <- fetchKSSL(series=c('sierra', 'amador'))
  f <- unique(toupper(x.multiple$taxonname)) %in% c('SIERRA', 'AMADOR')
  expect_true(all(f))
  
})

test_that("fetchKSSL() returns NULL with bogus query", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchKSSL(series='XXX'))
  expect_null(res)
  
})


test_that("fetchKSSL() fails gracefully when morphology data are missing", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  
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

test_that("fetchKSSL() geochem result", {
  
  # handy code snippet where res is KSSL data from mlra 17
  # dput(site(res$SPC)[unique(horizons(res$SPC)[horizons(res$SPC)$labsampnum %in% filter_geochem(res$geochem, 
  #                    major_element_method = "4H1b", trace_element_method = "4H1a")$labsampnum,]$pedon_key) %in%
  #                      site(res$SPC)$pedon_key,]$pedlabsampnum)
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') )  {
    skip("in-house testing only")
  }
  # get geochemical data for a single pedlabsampnum, do some basic filtering
  res <- fetchKSSL(pedlabsampnum = c("93P0249"), returnGeochemicalData = TRUE)
  
  expect_true(all(filter_geochem(res$geochem, prep_code='S')$prep_code == 'S'))
  
  expect_true(all(na.omit(filter_geochem(res$geochem, prep_code='S', 
                                         major_element_method = "4H1b", 
                                         trace_element_method = "4H1a")$prep_code == "S")))
  
  expect_true(all(na.omit(filter_geochem(res$geochem, 
                                          major_element_method = "4H1b", 
                                          trace_element_method = "4H1a")$prep_code == "S")))
})
