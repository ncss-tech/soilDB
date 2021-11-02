context("formatting multi-row records into single-row strings (from NASIS)")

## example data

# component parent material records for single component
# fairly typical example of COL/RES for single pmorigin
# pm order is populated which ensures consistent ordering of results
d.copm <- structure(
  list(
    coiid = c(1685719L, 1685719L),
    seqnum = c(NA_integer_,
               NA_integer_),
    pmorder = 1:2,
    pmdept_r = c(NA_integer_, NA_integer_),
    pmdepb_r = c(NA_integer_, NA_integer_),
    pmmodifier = c(NA_character_,
                   NA_character_),
    pmgenmod = c(NA, NA),
    pmkind = c("colluvium", "residuum"),
    pmorigin = c('metavolcanics', "metavolcanics")
  ),
  .Names = c(
    "coiid",
    "seqnum",
    "pmorder",
    "pmdept_r",
    "pmdepb_r",
    "pmmodifier",
    "pmgenmod",
    "pmkind",
    "pmorigin"
  ),
  row.names = 14:15,
  class = "data.frame"
)


test_that(".formatcoParentMaterialString functions correctly", {
  
  # attempt to flatten component parent material data into 2 strings
  # this is run on a single component's set of data
  res <- suppressWarnings(soilDB:::.formatcoParentMaterialString(d.copm))
  res2 <- soilDB:::.formatParentMaterialString(d.copm, uid = unique(d.copm$coiid), name.sep = "|")
  colnames(res2)[1] <- "coiid"
  
  # test for equal value
  expect_equal(res, res2)
  
  # there should only be a single row returned
  expect_equal(nrow(res2), 1)
  
  # required names
  expect_equal(names(res2), c('coiid', 'pmkind', 'pmorigin'))
  
  # data-check: single pmorigin, two pmkind: COL/RES
  expect_equal(res$pmkind, 'colluvium|residuum')
  expect_equal(res$pmorigin, 'metavolcanics')
  
})
