context("fetchOSD() -- requires internet connection")

## these are the elements of the list returned when extended=TRUE
## update here as-needed
extended.table.names <<- c("SPC", "competing", "geog_assoc_soils" ,"geomcomp", "hillpos", "mtnpos", "terrace", "flats", "shape_across", "shape_down", "pmkind", "pmorigin", "geomorphons", "mlra", "ecoclassid", "climate.annual", "climate.monthly", "NCCPI", "soilweb.metadata")

test_that("fetchOSD() works", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  ## sample data
  x <<- fetchOSD(soils = c('sierra', 'cecil'))
  
  skip_if(is.null(x))
  

  # standard request
  expect_true(inherits(x, 'SoilProfileCollection'))
  
  # all of the results should contain the search term
  f <- grepl('sierra|cecil', x$id, ignore.case = TRUE)
  expect_true(all(f))

})



test_that("fetchOSD() returns NULL with bogus query", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  # a message is printed and NULL returned when no results
  res <- suppressMessages(fetchOSD(soils='XXX'))
  expect_null(res)

})



test_that("fetchOSD() returns a list + SPC in extended mode", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  x.extended <<- fetchOSD(soils = c('sierra', 'cecil'), extended = TRUE)
  
  skip_if(is.null(x.extended))
  
  # extended request
  expect_true(inherits(x.extended, 'list'))
  expect_true(inherits(x.extended$SPC, 'SoilProfileCollection'))

})


test_that("fetchOSD() sensible values when missing in extended mode", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  x.extended <<- fetchOSD(soils = c('palau'), extended = TRUE)
  
  skip_if(is.null(x.extended))
  
  # extended request
  expect_false(x.extended$flats)
  
})



test_that("fetchOSD() returns reasonable data", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if(is.null(x))
  
  # standard request
  expect_equivalent(nrow(aqp::site(x)), 2)
  expect_gt(nrow(aqp::horizons(x)), 0)
  expect_equivalent(aqp::idname(x), 'id')
  expect_equivalent(aqp::horizonDepths(x), c("top", "bottom"))

})


test_that("fetchOSD() returns reasonable data in extended mode", {

  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if(is.null(x.extended))
  
  # extended request
  expect_named(x.extended, extended.table.names)

})


## pending further investigation due to FY22 changes: inactive series included in soilweb holdings
# # https://github.com/ncss-tech/soilDB/issues/128
# test_that("fetchOSD() returns warning in extended mode, when active + inactive series specified", {
# 
#   skip_if_offline()
# 
#   skip_on_cran()
#   
#   skip_if_not_installed("aqp")
# 
#   # warning added in response to #128
#   expect_warning(
#     xx <- fetchOSD(soils = c('sierra', 'Breadsprings', 'Hagerwest', 'Tintero'), extended = TRUE)
#   )
# 
#   # extended request components
#   expect_equal(
#     names(xx), extended.table.names
#   )
# 
# })


