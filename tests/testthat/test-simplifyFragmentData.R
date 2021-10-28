context("Simplification of fragment data (from NASIS)")

## related issues
# https://github.com/ncss-tech/soilDB/issues/57

## some complex data from NASIS phfrags table
d.single.hz <- structure(
  list(
    phiid = c(1202607L, 1202607L, 1202607L, 1202607L,
              1202607L),
    fragvol = c(5, 30, 10, 30, 5),
    fragsize_l = c(2L,
                   76L, 76L, 2L, 251L),
    fragsize_r = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_
    ),
    fragsize_h = c(75L, 250L,
                   250L, 75L, 600L),
    fragshp = structure(
      c(
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_
      ),
      .Label = c("flat", "nonflat"),
      class = "factor"
    ),
    fraghard = structure(
      c(10L, 2L, 10L, 2L,
        2L),
      .Label = c(
        "noncemented",
        "indurated",
        "moderately cemented",
        "strongly cemented",
        "weakly cemented",
        "extremely weakly",
        "very weakly",
        "very strongly",
        "weakly",
        "moderately",
        "strongly",
        "extremely strong",
        "H",
        "S"
      ),
      class = "factor"
    )
  ),
  .Names = c(
    "phiid",
    "fragvol",
    "fragsize_l",
    "fragsize_r",
    "fragsize_h",
    "fragshp",
    "fraghard"
  ),
  row.names = 306:310,
  class = "data.frame"
)

## data from NASIS phfrags with NA fragvol
d.missing.fragvol <- structure(
  list(
    phiid = c(1386592L, 1386592L, 1386592L, 1386592L,
              1386592L, 1386592L),
    fragvol = c(10, 10, 20, 20, 10, NA),
    fragsize_l = c(2L,
                   2L, 75L, 75L, 380L, NA),
    fragsize_r = c(
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_,
      NA_integer_
    ),
    fragsize_h = c(75L,
                   75L, 380L, 380L, 600L, NA),
    fragshp = structure(
      c(1L, 1L, 1L,
        1L, 1L, NA),
      .Label = c("flat", "nonflat"),
      class = "factor"
    ),
    fraghard = structure(
      c(11L, 9L, 11L, 9L, 11L, NA),
      .Label = c(
        "noncemented",
        "indurated",
        "moderately cemented",
        "strongly cemented",
        "weakly cemented",
        "extremely weakly",
        "very weakly",
        "very strongly",
        "weakly",
        "moderately",
        "strongly",
        "extremely strong",
        "H",
        "S"
      ),
      class = "factor"
    )
  ),
  .Names = c(
    "phiid",
    "fragvol",
    "fragsize_l",
    "fragsize_r",
    "fragsize_h",
    "fragshp",
    "fraghard"
  ),
  row.names = 1044:1049,
  class = "data.frame"
)

# all records are missing data
d.all.NA.fragvol <- d.missing.fragvol[6, ]


# no fragment size data, some records are NULL
d.missing.size <-
  structure(
    list(
      phiid = c(
        541527L,
        541528L,
        541529L,
        541530L,
        541543L,
        541544L,
        541545L,
        541546L,
        541547L,
        541548L,
        541549L,
        541550L
      ),
      fragvol = c(20, 30, 20, 8, 2, 2, 3, 5, 4, 4, NA, NA),
      fragsize_l = c(
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_
      ),
      fragsize_r = c(
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_
      ),
      fragsize_h = c(
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_
      ),
      fragshp = structure(
        c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        .Label = c("flat", "nonflat"),
        class = "factor"
      ),
      fraghard = structure(
        c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        .Label = c(
          "noncemented",
          "indurated",
          "moderately cemented",
          "strongly cemented",
          "weakly cemented",
          "extremely weakly",
          "very weakly",
          "very strongly",
          "weakly",
          "moderately",
          "strongly",
          "extremely strong",
          "H",
          "S"
        ),
        class = "factor"
      )
    ),
    .Names = c(
      "phiid",
      "fragvol",
      "fragsize_l",
      "fragsize_r",
      "fragsize_h",
      "fragshp",
      "fraghard"
    ),
    row.names = c(NA, 12L),
    class = "data.frame"
  )


test_that(".seive correctly skips / pads NA", {
  expect_equal(soilDB:::.sieve(diameter = c(NA, 55)), c(NA, 'gravel'))
})


test_that(".seive returns correct size class, nonflat, fragments", {
  
  expect_equal(soilDB:::.sieve(diameter = 4, flat = FALSE, para = FALSE), 'fine_gravel')
  expect_equal(soilDB:::.sieve(diameter = 6, flat = FALSE, para = FALSE), 'gravel')
  expect_equal(soilDB:::.sieve(diameter = 65, flat = FALSE, para = FALSE), 'gravel')
  expect_equal(soilDB:::.sieve(diameter = 74, flat = FALSE, para = FALSE), 'gravel')
  expect_equal(soilDB:::.sieve(diameter = 77, flat = FALSE, para = FALSE), 'cobbles')
  expect_equal(soilDB:::.sieve(diameter = 200, flat = FALSE, para = FALSE), 'cobbles')
  expect_equal(soilDB:::.sieve(diameter = 250, flat = FALSE, para = FALSE), 'stones')
  expect_equal(soilDB:::.sieve(diameter = 251, flat = FALSE, para = FALSE), 'stones')
  expect_equal(soilDB:::.sieve(diameter = 600, flat = FALSE, para = FALSE), 'boulders')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = FALSE, para = FALSE), 'boulders')
  expect_equal(soilDB:::.sieve(diameter = 900, flat = FALSE, para = FALSE), 'boulders')
  expect_equal(soilDB:::.sieve(diameter = 1000, flat = FALSE, para = FALSE), 'boulders')
})


test_that("seive returns correct size class, flat, fragments", {
  
  expect_equal(soilDB:::.sieve(diameter = 4, flat = TRUE, para = FALSE), 'channers')
  expect_equal(soilDB:::.sieve(diameter = 149, flat = TRUE, para = FALSE), 'channers')
  expect_equal(soilDB:::.sieve(diameter = 151, flat = TRUE, para = FALSE), 'flagstones')
  expect_equal(soilDB:::.sieve(diameter = 300, flat = TRUE, para = FALSE), 'flagstones')
  expect_equal(soilDB:::.sieve(diameter = 379, flat = TRUE, para = FALSE), 'flagstones')
  expect_equal(soilDB:::.sieve(diameter = 381, flat = TRUE, para = FALSE), 'stones')
  expect_equal(soilDB:::.sieve(diameter = 599, flat = TRUE, para = FALSE), 'stones')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = TRUE, para = FALSE), 'boulders')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = TRUE, para = FALSE), 'boulders')
  expect_equal(soilDB:::.sieve(diameter = 900, flat = TRUE, para = FALSE), 'boulders')
  
})


test_that("seive returns correct size class, nonflat, parafragments", {
  
  expect_equal(soilDB:::.sieve(diameter = 4, flat = FALSE, para = TRUE), 'parafine_gravel')
  expect_equal(soilDB:::.sieve(diameter = 6, flat = FALSE, para = TRUE), 'paragravel')
  expect_equal(soilDB:::.sieve(diameter = 65, flat = FALSE, para = TRUE), 'paragravel')
  expect_equal(soilDB:::.sieve(diameter = 77, flat = FALSE, para = TRUE), 'paracobbles')
  expect_equal(soilDB:::.sieve(diameter = 200, flat = FALSE, para = TRUE), 'paracobbles')
  expect_equal(soilDB:::.sieve(diameter = 249, flat = FALSE, para = TRUE), 'paracobbles')
  expect_equal(soilDB:::.sieve(diameter = 251, flat = FALSE, para = TRUE), 'parastones')
  expect_equal(soilDB:::.sieve(diameter = 599, flat = FALSE, para = TRUE), 'parastones')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = FALSE, para = TRUE), 'paraboulders')
  expect_equal(soilDB:::.sieve(diameter = 900, flat = FALSE, para = TRUE), 'paraboulders')
  expect_equal(soilDB:::.sieve(diameter = 1000, flat = FALSE, para = TRUE), 'paraboulders')
  
})


test_that("seive returns correct size class, flat, parafragments", {
  
  expect_equal(soilDB:::.sieve(diameter = 4, flat = TRUE, para = TRUE), 'parachanners')
  expect_equal(soilDB:::.sieve(diameter = 149, flat = TRUE, para = TRUE), 'parachanners')
  expect_equal(soilDB:::.sieve(diameter = 151, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_equal(soilDB:::.sieve(diameter = 300, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_equal(soilDB:::.sieve(diameter = 379, flat = TRUE, para = TRUE), 'paraflagstones')
  expect_equal(soilDB:::.sieve(diameter = 381, flat = TRUE, para = TRUE), 'parastones')
  expect_equal(soilDB:::.sieve(diameter = 599, flat = TRUE, para = TRUE), 'parastones')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = TRUE, para = TRUE), 'paraboulders')
  expect_equal(soilDB:::.sieve(diameter = 601, flat = TRUE, para = TRUE), 'paraboulders')
  expect_equal(soilDB:::.sieve(diameter = 900, flat = TRUE, para = TRUE), 'paraboulders')
  
})


## new tests for rockFragmentSieve: missing frag sizes / unspecified class
test_that("rockFragmentSieve puts fragments without fragsize into 'unspecified' class", {
  
  d <- data.frame(fragvol=25, fragsize_l=NA, fragsize_r=NA, fragsize_h=NA, fragshp=NA, fraghard=NA)
  res <- soilDB:::.rockFragmentSieve(d)
  
  expect_equal(res$class, 'unspecified')
  
})


test_that("rockFragmentSieve assumptions are applied, results correct", {
  
  d <- data.frame(fragvol=NA, fragsize_l=NA, fragsize_r=50, fragsize_h=NA, fragshp=NA, fraghard=NA)
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'gravel')
  
  # one more try
  d <- data.frame(fragvol=NA, fragsize_l=NA, fragsize_r=200, fragsize_h=NA, fragshp=NA, fraghard=NA)
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'cobbles')
  
})


test_that("rockFragmentSieve assumptions are applied when all NA", {
  
  d <- data.frame(fragvol=NA, fragsize_l=NA, fragsize_r=NA, fragsize_h=NA, fragshp=NA, fraghard=NA)
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # class should be NA
  expect_true(is.na(res$class))
  
})


test_that("rockFragmentSieve always uses the RV, computed when missing", {
  
  # full specification
  d <- data.frame(fragvol=10, fragsize_l=15, fragsize_r=50, fragsize_h=75, fragshp='nonflat', fraghard='strongly cemented')
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'gravel')
  
  # only RV available
  d <- data.frame(fragvol=10, fragsize_l=NA, fragsize_r=50, fragsize_h=NA, fragshp='nonflat', fraghard='strongly cemented')
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'gravel')
  
  # L/H available
  d <- data.frame(fragvol=10, fragsize_l=5, fragsize_r=NA, fragsize_h=74, fragshp='nonflat', fraghard='strongly cemented')
  res <- soilDB:::.rockFragmentSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$fragshp, 'nonflat')
  expect_equal(res$fraghard, 'strongly cemented')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'gravel')
  
})


test_that("rockFragmentSieve complex sample data from NASIS, single horizon", {
  
  # pretty common, many fragments specified for a single horizon
  res <- soilDB:::.rockFragmentSieve(d.single.hz)
  
  # correct classes
  expect_equal(res$class, c('cobbles', 'gravel', 'stones', 'paragravel', 'paracobbles'))
  
})


test_that("simplifyFragmentData complex sample data from NASIS, single horizon", {
  
  # pretty common, many fragments specified for a single horizon
  res <- soilDB::simplifyFragmentData(d.single.hz, id.var = 'phiid', nullFragsAreZero = TRUE)
  
  # correct class totals
  expect_equal(res$fine_gravel, 0)
  expect_equal(res$gravel, 30)
  expect_equal(res$cobbles, 30)
  expect_equal(res$stones, 5)
  expect_equal(res$paragravel, 5)
  expect_equal(res$paracobbles, 10)
  
  # correct total without parafrags
  expect_equal(res$total_frags_pct_nopf, 65)
  # correct total with parafrags
  expect_equal(res$total_frags_pct, 80)
  
})


test_that("simplifyFragmentData when missing fragment sizes, low/rv/high", {
  
  # all fragments are coallated into the unspecified column
  # totals should be correct
  # some horizons have no fragment records, should generate a warning
  expect_message( { res <- simplifyFragmentData(d.missing.size, id.var = 'phiid', nullFragsAreZero = TRUE) } )
  
  # rows missing fragvol should be removed from the simplified result
  expect_true(nrow(d.missing.size) == 12)
  expect_true(nrow(res) == 10)
  
  # unspecified total should match RF sums
  expect_equal(res$unspecified, res$total_frags_pct_nopf)
  expect_equal(res$unspecified, res$total_frags_pct)
})


test_that("simplifyFragmentData warning generated when NA in fragvol", {
  
  expect_message(simplifyFragmentData(d.missing.fragvol, id.var = 'phiid', nullFragsAreZero = TRUE), 
                 regexp = 'some records are missing rock fragment volume')
  
})


test_that("simplifyFragmentData warning generated when all fragvol are NA", {
  
  expect_message(simplifyFragmentData(d.all.NA.fragvol, id.var = 'phiid', nullFragsAreZero = TRUE), 
                 regexp = 'all records are missing rock fragment volume')
  
})


test_that("simplifyFragmentData nullFragsAreZero works as expected", {
  expect_message( { a <- simplifyFragmentData(d.missing.fragvol, id.var = 'phiid', nullFragsAreZero = FALSE) } )
  expect_message( { b <- simplifyFragmentData(d.missing.fragvol, id.var = 'phiid', nullFragsAreZero = TRUE) } )
  expect_equal(as.logical(is.na(a)), 
               c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, 
                           FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_true(all(!is.na(b)))
})

