context("EDIT web services")

test_that("make_EDIT_service_URL works", {
  
  # test simple construction of URLs for https://edit.jornada.nmsu.edu/services/...
  #  return PDF, .TXT or JSON result
  
  # url for all geoUnit keys as PDF
  expect_equal(make_EDIT_service_URL(src = "descriptions",
                        catalog = "esd",
                        geoUnit = "039X"), 
               "https://edit.jornada.nmsu.edu/services/descriptions/esd/039X.pdf")
  
  # url for a single key within geoUnit as PDF
  expect_equal(make_EDIT_service_URL(src = "descriptions",
                        catalog = "esd",
                        geoUnit = "039X",
                        key = "1"), 
               "https://edit.jornada.nmsu.edu/services/descriptions/esd/039X/1.pdf")
  
  # query for ecoclass with endpoint "overview"
  expect_equal(make_EDIT_service_URL(src = "descriptions",
                                   catalog = "esd",
                                   geoUnit = "039X",
                                   ecoclass = "R039XA109AZ",
                                   endpoint = "overview.json"),
               "https://edit.jornada.nmsu.edu/services/descriptions/esd/039X/R039XA109AZ/overview.json")
  
})


test_that("get_EDIT_ecoclass_by_geoUnit works", {
  
  # wrapper method to download class-list.json for
  #  multiple geoUnit and return a "tidy" data.frame result instead of nested list
  
  skip_if_offline()
  
  skip_on_cran()
  
  res <- get_EDIT_ecoclass_by_geoUnit(c("018X","022A"))
  
  # skip on error
  skip_if(is.null(res))
  
  # verify data.frame result with 4 columns as specified @ https://edit.jornada.nmsu.edu/resources/esd/
  expect_true(all(colnames(res) %in% c("geoUnit", "id", "legacyId", "name")))
  
})
