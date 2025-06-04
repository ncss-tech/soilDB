context("fetchKSSL() -- requires internet connection")

test_that("fetchKSSL() works", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")

  skip_if_not_installed("farver")

  ## sample data
  x <<- try(fetchKSSL(series = 'sierra'), silent = TRUE)

  skip_if(inherits(x, 'try-error') || is.null(x))

  # standard request
  expect_true(inherits(x, 'SoilProfileCollection'))

})


test_that("fetchKSSL() returns an SPC or list", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("farver")

  x.morph <<- fetchKSSL(series = 'sierra',
                        returnMorphologicData = TRUE,
                        progress = FALSE)

  x.morp.simple.colors <<- fetchKSSL(series = 'sierra',
                                     returnMorphologicData = TRUE,
                                     simplifyColors = TRUE,
                                     progress = FALSE)

  skip_if(inherits(x.morph, 'try-error') || is.null(x.morph))

  skip_if(inherits(x.morp.simple.colors, 'try-error') || is.null(x.morp.simple.colors))


  # SPC + morphologic data
  expect_true(inherits(x.morph, 'list'))
  expect_true(inherits(x.morph$SPC, 'SoilProfileCollection'))
  expect_true(inherits(x.morph$morph, 'list'))

  # simplified colors, merges into @horizons
  expect_false(is.null(x.morp.simple.colors$SPC$moist_soil_color))

})


test_that("fetchKSSL() returns reasonable data", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("farver")

  skip_if(inherits(x, 'try-error') || is.null(x))

  # standard request
  expect_true(nrow(aqp::site(x)) > 0)
  expect_true(aqp::nrow(x) > 0)
  expect_identical(aqp::idname(x), 'pedon_key')
  expect_identical(aqp::horizonDepths(x), c("hzn_top", "hzn_bot"))

})

test_that("fetchKSSL() returns data associated with named series (sierra)", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("farver")

  skip_if(inherits(x, 'try-error') || is.null(x))

  # all of the results should contain the search term
  f <- grepl('sierra', x$taxonname, ignore.case = TRUE)
  expect_true(all(f))

})


test_that("fetchKSSL() returns data associated with multiple named series", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  x.multiple <- fetchKSSL(series = c('sierra', 'amador'), progress = FALSE)

  skip_if(inherits(x.multiple, 'try-error') || is.null(x.multiple))

  f <- unique(toupper(x.multiple$taxonname)) %in% c('SIERRA', 'AMADOR')
  expect_true(all(f))

})

test_that("fetchKSSL() returns NULL with bogus query", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchKSSL(series = 'XXX'))

  expect_null(res)

})


test_that("fetchKSSL() fails gracefully when morphology data are missing", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  # pedon_key 37457 is missing:
  # * most lab data
  # * all morphologic data
  # --> cannot simplify colors, so skip
  res <- suppressMessages(
    fetchKSSL(
      pedon_key = 37457,
      returnMorphologicData = TRUE,
      simplifyColors = TRUE,
      progress = FALSE
    )
  )

  skip_if(inherits(res, 'try-error') || is.null(res))

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

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  # get geochemical data for a single pedlabsampnum, do some basic filtering
  res <- try(fetchKSSL(pedlabsampnum = c("93P0249"), returnGeochemicalData = TRUE, progress = FALSE))

  skip_if(inherits(res, 'try-error') || is.null(res))

  expect_true(all(filter_geochem(res$geochem, prep_code = 'S')$prep_code == 'S'))

  expect_true(all(na.omit(filter_geochem(res$geochem, prep_code = 'S',
                                         major_element_method = "4H1b",
                                         trace_element_method = "4H1a")$prep_code == "S")))

  expect_true(all(na.omit(filter_geochem(res$geochem,
                                          major_element_method = "4H1b",
                                          trace_element_method = "4H1a")$prep_code == "S")))

  # try an ID without geochem data
  res <- try(fetchKSSL(pedlabsampnum = "05N0025", returnGeochemicalData = TRUE), silent = TRUE)

  skip_if(inherits(res, 'try-error') || is.null(res))

  # should be a data.frame, even when missing data
  # it is a 0-length data.frame
  expect_true(inherits(res$geochem, 'data.frame'))
  expect_true(inherits(res$optical, 'data.frame'))

})
