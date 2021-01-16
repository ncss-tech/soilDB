context("fetchNASISWebReport() -- requires internet connection")

test_that("fetchNASISWebReport() works", {

  skip_if_offline()

  skip_on_cran()

  ## sample data
  pn <- "MLRA 18 - Sierra sandy loam, 2 to 9 percent slopes"

  x <<- suppressMessages(fetchNASISWebReport(projectname = pn))

  # standard request
  expect_true(inherits(x, 'list'))

})


test_that("fetchNASISWebReport() returns an SPC of component/horizon data and data.frame of mapunits", {

  skip_if_offline()

  skip_on_cran()

  # SPC + morphologic data
  expect_true(inherits(x$spc, 'SoilProfileCollection'))
  expect_true(inherits(x$mapunit, 'data.frame'))

})


test_that("fetchNASISWebReport() returns reasonable data", {

  skip_if_offline()

  skip_on_cran()

  # standard request
  expect_equal(nrow(site(x$spc)) > 0, TRUE)
  expect_equal(nrow(horizons(x$spc)) > 0, TRUE)
  expect_equal(idname(x$spc), 'coiid')
  expect_equal(hzidname(x$spc), 'chiid')
  expect_equal(horizonDepths(x$spc), c("hzdept_r", "hzdepb_r"))

})

test_that("fetchNASISWebReport() returns data for component name (Sierra)", {

  skip_if_offline()

  skip_on_cran()

  # all major components are Sierra
  f <- grepl('Sierra', x$spc$compname[x$spc$majcompflag == 1], ignore.case = TRUE)
  expect_equal(all(f), TRUE)

})


test_that("fetchNASISWebReport() returns NULL with bogus query", {

  skip_if_offline()

  skip_on_cran()

  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchNASISWebReport(projectname='XXX'))
  expect_null(res)

})

