context("Simplification of fragment data (from NASIS)")


test_that(".seive correctly skips / pads NA", {
  expect_equal(soilDB:::.sieve(diameter = c(NA, 55)), c(NA, 'gravel'))
})


test_that(".seive returns correct size class, nonflat, fragments", {
  
  expect_match(soilDB:::.sieve(diameter = 4, flat = FALSE, para = FALSE), 'fine_gravel')
  expect_match(soilDB:::.sieve(diameter = 6, flat = FALSE, para = FALSE), 'gravel')
  expect_match(soilDB:::.sieve(diameter = 65, flat = FALSE, para = FALSE), 'gravel')
  expect_match(soilDB:::.sieve(diameter = 77, flat = FALSE, para = FALSE), 'cobbles')
  expect_match(soilDB:::.sieve(diameter = 200, flat = FALSE, para = FALSE), 'cobbles')
  expect_match(soilDB:::.sieve(diameter = 250, flat = FALSE, para = FALSE), 'cobbles')
  expect_match(soilDB:::.sieve(diameter = 251, flat = FALSE, para = FALSE), 'stones')
  expect_match(soilDB:::.sieve(diameter = 600, flat = FALSE, para = FALSE), 'stones')
  expect_match(soilDB:::.sieve(diameter = 601, flat = FALSE, para = FALSE), 'boulders')
  expect_match(soilDB:::.sieve(diameter = 900, flat = FALSE, para = FALSE), 'boulders')
  expect_match(soilDB:::.sieve(diameter = 1000, flat = FALSE, para = FALSE), 'boulders')
})


test_that("seive returns correct size class, flat, fragments", {
  
  expect_match(soilDB:::.sieve(diameter = 4, flat = TRUE, para = FALSE), 'channers')
  expect_match(soilDB:::.sieve(diameter = 150, flat = TRUE, para = FALSE), 'channers')
  expect_match(soilDB:::.sieve(diameter = 151, flat = TRUE, para = FALSE), 'flagstones')
  expect_match(soilDB:::.sieve(diameter = 300, flat = TRUE, para = FALSE), 'flagstones')
  expect_match(soilDB:::.sieve(diameter = 380, flat = TRUE, para = FALSE), 'flagstones')
  expect_match(soilDB:::.sieve(diameter = 381, flat = TRUE, para = FALSE), 'stones')
  expect_match(soilDB:::.sieve(diameter = 600, flat = TRUE, para = FALSE), 'stones')
  expect_match(soilDB:::.sieve(diameter = 601, flat = TRUE, para = FALSE), 'boulders')
  expect_match(soilDB:::.sieve(diameter = 601, flat = TRUE, para = FALSE), 'boulders')
  expect_match(soilDB:::.sieve(diameter = 900, flat = TRUE, para = FALSE), 'boulders')
  
})



test_that("seive returns correct size class, nonflat, parafragments", {
  
  expect_match(soilDB:::.sieve(diameter = 4, flat = FALSE, para = TRUE), 'parafine_gravel')
  expect_match(soilDB:::.sieve(diameter = 6, flat = FALSE, para = TRUE), 'paragravel')
  expect_match(soilDB:::.sieve(diameter = 65, flat = FALSE, para = TRUE), 'paragravel')
  expect_match(soilDB:::.sieve(diameter = 77, flat = FALSE, para = TRUE), 'paracobbles')
  expect_match(soilDB:::.sieve(diameter = 200, flat = FALSE, para = TRUE), 'paracobbles')
  expect_match(soilDB:::.sieve(diameter = 250, flat = FALSE, para = TRUE), 'paracobbles')
  expect_match(soilDB:::.sieve(diameter = 251, flat = FALSE, para = TRUE), 'parastones')
  expect_match(soilDB:::.sieve(diameter = 600, flat = FALSE, para = TRUE), 'parastones')
  expect_match(soilDB:::.sieve(diameter = 601, flat = FALSE, para = TRUE), 'paraboulders')
  expect_match(soilDB:::.sieve(diameter = 900, flat = FALSE, para = TRUE), 'paraboulders')
  expect_match(soilDB:::.sieve(diameter = 1000, flat = FALSE, para = TRUE), 'paraboulders')
  
})


test_that("seive returns correct size class, flat, parafragments", {
  
  expect_match(soilDB:::.sieve(diameter = 4, flat = TRUE, para = TRUE), 'parachanners')
  expect_match(soilDB:::.sieve(diameter = 150, flat = TRUE, para = TRUE), 'parachanners')
  expect_match(soilDB:::.sieve(diameter = 151, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_match(soilDB:::.sieve(diameter = 300, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_match(soilDB:::.sieve(diameter = 380, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_match(soilDB:::.sieve(diameter = 381, flat = TRUE, para = TRUE), 'parastones')
  expect_match(soilDB:::.sieve(diameter = 600, flat = TRUE, para = TRUE), 'parastones')
  expect_match(soilDB:::.sieve(diameter = 601, flat = TRUE, para = TRUE), 'paraboulders')
  expect_match(soilDB:::.sieve(diameter = 601, flat = TRUE, para = TRUE), 'paraboulders')
  expect_match(soilDB:::.sieve(diameter = 900, flat = TRUE, para = TRUE), 'paraboulders')
  
})

